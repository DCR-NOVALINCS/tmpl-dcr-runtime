open Helper
open Ast
open Syntax
open Error
open Errors
open Unparser
open Common
open Monads.ResultMonad
open Env
open Printing

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Entry point                                                              │
   └──────────────────────────────────────────────────────────────────────────┘ *)

let rec typecheck ?(event_env = empty_env) program =
  let template_decls = program.template_decls in
  let events, insts, relations, annotations =
    ( program.events
    , program.template_insts
    , program.relations
    , program.annotations )
  in
  let ty_env, label_types = (empty_env, EventTypes.empty) in
  (* Don't know why, after creating a empty hashtable, needs to reset... *)
  EventTypes.reset label_types ;
  typecheck_template_decls template_decls (ty_env, event_env, label_types)
  >>= fun (ty_env, event_env, tmpl_ty_env, label_types) ->
  typecheck_subprogram
    (events, insts, relations, annotations)
    (ty_env, event_env, tmpl_ty_env, label_types)

(* =============================================================================
   Typechecking of template definitions
   ============================================================================= *)

and typecheck_template_decls template_decls ?(tmpl_ty_env = empty_env)
    (ty_env, event_env, label_types) =
  fold_left
    (fun (ty_env, event_env, tmpl_ty_env, label_types) template_decl ->
      typecheck_template_decl template_decl
        (ty_env, event_env, tmpl_ty_env, label_types) )
    (ty_env, event_env, tmpl_ty_env, label_types)
    template_decls

and typecheck_template_decl template_decl
    (ty_env, event_env, tmpl_ty_env, label_types) =
  let { graph= events, insts, relations, annotations
      ; export
      ; export_types
      ; params
      ; id
      ; _ } =
    template_decl
  and typecheck_params params (ty_env, event_env, label_types) =
    let typecheck_param (pid, ty, default) (ty_env, event_env, label_types) =
      let* ty =
        match default with
        | None -> return ty.data
        | Some expr ->
            typecheck_expr ~ty_env expr
            >>= fun default_ty ->
            if equal_types ty.data default_ty then return ty.data
            else type_mismatch ~loc:expr.loc [ty.data] [default_ty]
      in
      let ty_env = bind pid.data ty ty_env in
      let* event_env, label_types =
        match ty with
        | EventTy label ->
            ( match EventTypes.find label.data label_types with
            | None ->
                (* FIXME: In case of not found the label in this point of the program, what to do? *)
                let label_types =
                  EventTypes.add (label.data, Undefined) label_types
                in
                return
                  ( Output (annotate ~ty:(Some UnitTy) (default_value UnitTy))
                  , label_types )
            | Some Undefined ->
                Logger.debug
                @@ Printf.sprintf "Label %s is undefined" label.data ;
                return
                  ( Output (annotate ~ty:(Some UnitTy) (default_value UnitTy))
                  , label_types )
                (* return (Input (annotate UnitTy), label_types, label :: remaining) *)
            | Some (Defined (value_ty, event_type)) -> (
              match event_type with
              | InputType -> return (Input (annotate value_ty), label_types)
              | OutputType ->
                  return
                    ( Output
                        (annotate ~ty:(Some value_ty) (default_value value_ty))
                    , label_types )
                  (* FIXME: Get a value of the output event *) ) )
            >>= fun (event_io, label_types) ->
            let event = mk_event (pid, label) (annotate event_io) in
            return (bind pid.data event event_env)
            >>= fun event_env -> return (event_env, label_types)
        | _ -> return (event_env, label_types)
      in
      return (ty_env, event_env, label_types)
    in
    fold_right
      (fun (ty_env, event_env, label_types) param ->
        typecheck_param param (ty_env, event_env, label_types) )
      (ty_env, event_env, label_types)
      params
  in
  Logger.info ("Typechecking template: " ^ keyword id.data) ;
  (* Begin new scope *)
  return (begin_scope ty_env, begin_scope event_env)
  >>= fun (ty_env, event_env) ->
  (* Typecheck the parameters *)
  typecheck_params params (ty_env, event_env, label_types)
  >>= fun (ty_env, event_env, label_types) ->
  (* Bind the events from the template into the env's *)
  (* bind_events ~f:(fun event -> event) events event_env *)
  fold_left
    (fun event_env event ->
      let {info= id, _; _} = event.data in
      return (bind id.data event event_env) )
    event_env events
  >>= fun event_env ->
  (* Typecheck the graph of the template *)
  typecheck_subprogram
    (events, insts, relations, annotations)
    (ty_env, event_env, tmpl_ty_env, label_types)
  >>= fun (_tmpl_ty_env, tmpl_event_env, label_types) ->
  (* Check the exported events and their typings *)
  partition_map
    (fun export_id ->
      match find_flat export_id.data tmpl_event_env with
      | None -> Right export_id
      | Some event -> Left event )
    export
  >>= fun (exported_events, event_ids_not_found) ->
  if not (List.is_empty event_ids_not_found) then
    (* Error when some exported events are not found *)
    let event_id = List.hd event_ids_not_found in
    if List.length event_ids_not_found = 1 then
      event_not_found ~loc:event_id.loc event_id.data
    else
      (* FIXME: make the location of all elements of the list *)
      events_not_found ~loc:event_id.loc event_ids_not_found
  else if
    (not (List.length exported_events = List.length export))
    || not (List.length exported_events = List.length export_types)
  then
    (* Error when the number of exported events does not match the number of export types *)
    missing_exported_event_types ~expected:export_types
      (List.map
         (fun e ->
           let {info= id, _; _} = e.data in
           id )
         exported_events )
  else
    return @@ List.combine exported_events export_types
    >>= fun exported_type_mapping ->
    iter
      (fun (event, label_ty) ->
        let {info= _, label; _} = event.data in
        if label.data = label_ty.data then return ()
        else type_mismatch ~loc:label_ty.loc [EventTy label_ty] [EventTy label] )
      exported_type_mapping
    >>= fun _ ->
    return (mk_template_ty_from template_decl)
    >>= fun tmpl_ty ->
    return (bind id.data tmpl_ty tmpl_ty_env)
    >>= fun tmpl_ty_env ->
    return (end_scope ty_env, end_scope event_env, tmpl_ty_env, label_types)

(* =============================================================================
   Typechecking of subprograms
   ============================================================================= *)

and typecheck_subprogram (events, insts, relations, annotations)
    (ty_env, event_env, tmpl_ty_env, label_types) =
  (* Typecheck events *)
  typecheck_events events (ty_env, event_env, label_types)
  >>= fun (ty_env, event_env, label_types) ->
  (* Typecheck template instantiations *)
  typecheck_insts insts (ty_env, event_env, tmpl_ty_env, label_types)
  >>= fun (ty_env, event_env, label_types) ->
  (* Typecheck relations *)
  typecheck_relations relations (ty_env, event_env, tmpl_ty_env, label_types)
  >>= fun (ty_env, event_env, label_types) ->
  (* Typecheck template annotations *)
  typecheck_template_annotations annotations
    (ty_env, event_env, tmpl_ty_env, label_types)
  >>= fun (ty_env, event_env, label_types) ->
  (* Debug envs *)
  Logger.debug @@ "Type env after typechecking subprogram:\n"
  ^ string_of_env Plain.unparse_ty ty_env ;
  Logger.debug @@ "Event env after typechecking subprogram:\n"
  ^ string_of_env (fun e -> Plain.unparse_events [e]) event_env ;
  return (ty_env, event_env, label_types)

(* =============================================================================
   Typechecking of events
   ============================================================================= *)

and typecheck_events events (ty_env, event_env, label_types) =
  fold_right
    (fun (ty_env, event_env, label_types) event ->
      typecheck_event event (ty_env, event_env, label_types) )
    (ty_env, event_env, label_types)
    events

and typecheck_event event (ty_env, event_env, label_types) =
  let {info= id, label; io; marking; _} = event.data in
  ( match io.data with
  | Input ty -> return (ty.data, InputType)
  | Output expr ->
      typecheck_expr ~ty_env expr >>= fun ty -> return (ty, OutputType) )
  >>= fun (got_value_ty, got_event_type) ->
  ( match EventTypes.find label.data label_types with
  | None | Some Undefined ->
      Logger.debug
      @@ Printf.sprintf "%s -> %s" label.data
           (show_event_type' (Plain.unparse_ty got_value_ty) got_event_type) ;
      return
        (EventTypes.add
           (label.data, Defined (got_value_ty, got_event_type))
           label_types )
  | Some (Defined (expected_value_ty, expected_event_type)) ->
      if
        equal_types got_value_ty expected_value_ty
        && got_event_type = expected_event_type
      then return label_types
      else
        event_type_mismatch ~loc:io.loc
          ~available:(EventTypes.to_list label_types)
          [(label.data, expected_event_type, expected_value_ty)]
          [(label.data, got_event_type, got_value_ty)] )
  >>= fun label_types ->
  !(marking.data.value).ty := Some got_value_ty ;
  let event_ty = event_as_ty event in
  io.ty := Some event_ty ;
  let ty_env = bind id.data event_ty ty_env in
  let event_env = bind id.data event event_env in
  return (ty_env, event_env, label_types)

(* =============================================================================
   Typechecking of template instances
   ============================================================================= *)

and typecheck_insts insts (ty_env, event_env, tmpl_ty_env, label_types) =
  fold_right
    (fun (ty_env, event_env, label_types) inst ->
      typecheck_inst inst (ty_env, event_env, tmpl_ty_env, label_types) )
    (ty_env, event_env, label_types)
    insts

and typecheck_inst inst (ty_env, event_env, tmpl_ty_env, label_types) =
  let {args; x; tmpl_id; _} = inst.data in
  (* Find the template used *)
  match find_flat tmpl_id.data tmpl_ty_env with
  | None -> tmpl_not_found tmpl_id
  | Some template_ty ->
      let {expr_param_tys; event_param_labels; export_tys} = template_ty in
      let typecheck_args args (ty_env, event_env, label_types) =
        fold_left
          (fun (ty_env, event_env, label_types) (id, expr) ->
            typecheck_expr ~ty_env ~label_types expr
            >>= fun got_ty ->
            match
              ( List.assoc_opt id.data expr_param_tys
              , List.assoc_opt id.data event_param_labels )
            with
            | None, None ->
                fixme ~loc:id.loc
                  (Printf.sprintf "id %s from param not found" (keyword id.data))
            | Some expected_ty, _ ->
                if equal_types got_ty expected_ty.data then
                  return (ty_env, event_env, label_types)
                else type_mismatch ~loc:expr.loc [expected_ty.data] [got_ty]
            | _, Some label -> (
              match EventTypes.find label.data label_types with
              | None -> id_not_found label
              | Some Undefined -> return (ty_env, event_env, label_types)
              | Some (Defined (_value_ty, event_type)) -> (
                match event_type with
                | InputType -> return (ty_env, event_env, label_types)
                | OutputType -> return (ty_env, event_env, label_types) ) ) )
          (ty_env, event_env, label_types)
          args
      (* fold_left
           (fun (ty_env, event_env, label_types) (id, arg_type) ->
             match arg_type with
             | ExprArg expr -> (
                 typecheck_expr ~ty_env expr
                 >>= fun got_ty ->
                 match List.assoc_opt id.data expr_param_tys with
                 | None -> id_not_found id
                 | Some expected_ty ->
                     if equal_types got_ty expected_ty.data then
                       return (ty_env, event_env, label_types)
                     else type_mismatch ~loc:expr.loc [expected_ty.data] [got_ty]
                 (* >>= fun _ -> return (ty_env, event_env) *) )
             | EventArg event_id -> (
               match find_flat event_id.data event_env with
               | None -> id_not_found event_id
               | Some event ->
                   typecheck_event event (ty_env, event_env, label_types)
                   >>= fun (ty_env, event_env, label_types) ->
                   return (ty_env, event_env, label_types) ) )
           (ty_env, event_env, label_types)
           args *)
      and make_x_events xs (ty_env, event_env, label_types) =
        if not (List.length xs = List.length export_tys) then
          missing_exported_events
            ~expected:(List.map (fun e -> annotate (fst e)) export_tys)
            xs
        else
          iter
            (fun x ->
              match find_flat x.data event_env with
              | None -> return ()
              | Some _ -> duplicate_event x )
            xs
          >>= fun _ ->
          return @@ List.combine xs (List.map snd export_tys)
          >>= fun xs ->
          (* Binds exported events to be used in the latter *)
          fold_left
            (fun (ty_env, event_env, label_types) (x, label) ->
              ( match EventTypes.find label.data label_types with
              | None ->
                  missing_label
                    ~available_labels:
                      ( EventTypes.to_list label_types
                      |> List.map (fun (x, _) -> annotate x) )
                    label
              | Some Undefined ->
                  let label_types = EventTypes.remove label.data label_types in
                  return
                    ( Output (annotate ~ty:(Some UnitTy) (default_value UnitTy))
                    , label_types )
              | Some (Defined (value_ty, event_type)) -> (
                match event_type with
                | InputType ->
                    return
                      ( Input (annotate ~loc:x.loc ~ty:(Some value_ty) value_ty)
                      , label_types )
                | OutputType ->
                    return
                      ( Output
                          (annotate ~loc:x.loc ~ty:(Some value_ty)
                             (default_value value_ty) )
                      , label_types ) ) )
              >>= fun (event_io, label_types) ->
              let x_event = mk_event (x, label) (annotate event_io) in
              (* bind_events ~f:event_as_ty [x_event] ty_env *)
              return (bind x.data x_event event_env)
              >>= fun event_env ->
              (* bind_events ~f:(fun e -> e) [x_event] event_env *)
              return (bind x.data (event_as_ty x_event) ty_env)
              >>= fun ty_env ->
              typecheck_event x_event (ty_env, event_env, label_types) )
            (ty_env, event_env, label_types)
            xs
      in
      typecheck_args args (ty_env, event_env, label_types)
      >>= fun (ty_env, event_env, label_types) ->
      make_x_events x (ty_env, event_env, label_types)

(* =============================================================================
   Typechecking of relations
   ============================================================================= *)

and typecheck_relations relations (ty_env, event_env, tmpl_ty_env, label_types)
    =
  fold_right
    (fun (ty_env, event_env, label_types) relation ->
      typecheck_relation relation (ty_env, event_env, tmpl_ty_env, label_types) )
    (ty_env, event_env, label_types)
    relations

and typecheck_relation relation (ty_env, event_env, tmpl_ty_env, label_types) =
  let check_event_id id =
    match find_flat id.data event_env with
    | None ->
        Logger.debug @@ "Event env when [check_event] " ^ id.data ^ ":\n"
        ^ string_of_env Colorized.unparse_event event_env ;
        id_not_found id
    | Some event -> return event
  and check_guard_expr guard =
    let* guard_ty = typecheck_expr ~ty_env guard in
    if equal_types guard_ty BoolTy then return BoolTy
    else type_mismatch ~loc:guard.loc [BoolTy] [guard_ty]
  in
  match relation.data with
  | ControlRelation (from_id, guard, dest, _) ->
      let* _ = check_event_id from_id in
      let* _ = check_event_id dest in
      let* _ = check_guard_expr guard in
      return (ty_env, event_env, label_types)
  | SpawnRelation (from_id, guard, subprogram) ->
      let* from_event = check_event_id from_id in
      let* _ = check_guard_expr guard in
      let spawn_event_env, spawn_ty_env =
        ( begin_scope event_env |> bind trigger_id from_event
        , begin_scope ty_env |> bind trigger_id (event_as_ty from_event) )
      in
      let* ty_env, event_env, label_types =
        typecheck_subprogram subprogram
          (spawn_ty_env, spawn_event_env, tmpl_ty_env, label_types)
      in
      return (end_scope ty_env, end_scope event_env, label_types)

(* =============================================================================
   Typechecking of template annotations
   ============================================================================= *)

and typecheck_template_annotations annotations
    (ty_env, event_env, tmpl_ty_env, label_types) =
  fold_right
    (fun (ty_env, event_env, label_types) annotation ->
      typecheck_template_annotation annotation
        (ty_env, event_env, tmpl_ty_env, label_types) )
    (ty_env, event_env, label_types)
    annotations

and typecheck_template_annotation annotation
    (ty_env, event_env, tmpl_ty_env, label_types) =
  match annotation with
  | IfElse {condition; then_branch; else_branch} ->
      let* condition_ty = typecheck_expr ~ty_env ~label_types condition in
      let* branch_ty_env, branch_event_env =
        if equal_types condition_ty BoolTy then
          return (begin_scope ty_env, begin_scope event_env)
        else type_mismatch ~loc:condition.loc [BoolTy] [condition_ty]
      in
      let* _, _, label_types =
        typecheck_subprogram then_branch
          (branch_ty_env, branch_event_env, tmpl_ty_env, label_types)
      in
      let* _, _, label_types =
        match else_branch with
        | None -> return (branch_ty_env, branch_event_env, label_types)
        | Some else_body ->
            typecheck_subprogram else_body
              (branch_ty_env, branch_event_env, tmpl_ty_env, label_types)
      in
      return (ty_env, event_env, label_types)
  | Foreach (id, expr, body) ->
      let* expr_ty = typecheck_expr expr ~ty_env in
      let* annot_ty_env, annot_event_env, label_types =
        match expr_ty with
        | ListTy elem_ty ->
            let ty_env, event_env =
              (begin_scope ty_env |> bind id.data elem_ty, begin_scope event_env)
            in
            return (ty_env, event_env, label_types)
        | _ -> type_mismatch ~loc:expr.loc [ListTy UnitTy] [expr_ty]
      in
      Logger.debug
        (Printf.sprintf "Annotation envs:\n%s\n%s"
           (string_of_env Plain.unparse_ty annot_ty_env)
           (string_of_env (fun e -> Plain.unparse_events [e]) annot_event_env) ) ;
      let* _, _, label_types =
        typecheck_subprogram body
          (annot_ty_env, annot_event_env, tmpl_ty_env, label_types)
      in
      return (ty_env, event_env, label_types)

(* =============================================================================
   Typechecking of expressions
   ============================================================================= *)

and typecheck_expr ?(ty_env = empty_env) ?(label_types = EventTypes.empty) expr
    =
  (* Typechecking a binary operation, e.g +, *, etc. *)
  let typecheck_binop l_ty r_ty op =
    ( match op with
    | Add | Sub | Mult | Div -> return (IntTy, IntTy, IntTy)
    | And | Or -> return (BoolTy, BoolTy, BoolTy)
    | Eq | NotEq -> return (l_ty, l_ty, BoolTy)
    | GreaterThan | GreaterOrEqual | LessThan | LessOrEqual ->
        return (IntTy, IntTy, BoolTy) )
    >>= fun (expected_l_ty, expected_r_ty, expected_result_ty) ->
    if equal_types expected_l_ty l_ty && equal_types expected_r_ty r_ty then
      return expected_result_ty
    else type_mismatch ~loc:expr.loc [expected_l_ty; expected_r_ty] [l_ty; r_ty]
  (* Typechecking a unary operation, e.g -, ~, etc. *)
  and typecheck_unop e_ty op =
    ( match op with
    | Negation -> return (BoolTy, BoolTy)
    | Minus -> return (IntTy, IntTy) )
    >>= fun (expected_e_ty, expected_result_ty) ->
    if e_ty = expected_e_ty then return expected_result_ty
    else type_mismatch ~loc:expr.loc [expected_e_ty] [e_ty]
  (* Typechecking a identifier *)
  and typecheck_identifier id ty_env =
    match find_flat id.data ty_env with
    | None ->
        (* Logger.debug @@ "Type env:\n"
           ^ string_of_env unparse_ty ty_env ; *)
        id_not_found id
    | Some ty -> return ty
  (* Typechecking a record' property *)
  and typecheck_prop r_ty p =
    match r_ty with
    | RecordTy fields -> (
        let fields = List.map (fun (p, ty) -> (deannotate p, ty)) fields in
        match List.assoc_opt p.data fields with
        | Some ty -> return ty.data
        | None -> property_not_found_type p r_ty )
    | EventTy label -> (
      match EventTypes.find label.data label_types with
      | None ->
          missing_label
            (* ~available_labels:(EventTypes.to_list label_types) *)
            label
      | Some Undefined -> return UnitTy
      | Some (Defined (value_ty, event_type)) -> (
        match event_type with
        | InputType -> return value_ty
        | OutputType -> return value_ty ) )
    | _ -> type_mismatch ~loc:expr.loc [RecordTy []] [r_ty]
  (* Typechecking a list *)
  and typecheck_list lst ty_env =
    (* Typechecking each element of the list [lst] *)
    map (fun e -> typecheck_expr ~ty_env e) lst
    >>= fun elem_tys ->
    if List.is_empty elem_tys then return (ListTy UnitTy)
    else
      (* Check if all elements have the same type *)
      let expected_ty = List.hd elem_tys in
      partition_map
        (fun elem_ty ->
          if equal_types elem_ty expected_ty then Left elem_ty
          else Right elem_ty )
        elem_tys
      >>= fun (_, wrong_tys) ->
      if List.is_empty wrong_tys then return (ListTy expected_ty)
      else type_mismatch ~loc:expr.loc [expected_ty] wrong_tys
  (* Typechecking a record *)
  and typecheck_record fields ty_env =
    map
      (fun (prop, expr) ->
        typecheck_expr ~ty_env expr
        >>= fun ty -> return (prop, annotate ~loc:expr.loc ty) )
      fields
    >>= fun fields -> return (RecordTy fields)
  in
  let {data; ty; loc} = expr in
  ( match data with
  | Unit -> return UnitTy
  | BoolLit _ -> return BoolTy
  | IntLit _ -> return IntTy
  | StringLit _ -> return StringTy
  | Parenthesized e -> typecheck_expr ~ty_env e
  | BinaryOp (l, r, op) ->
      typecheck_expr ~ty_env l
      >>= fun l_ty ->
      typecheck_expr ~ty_env r >>= fun r_ty -> typecheck_binop l_ty r_ty op
  | UnaryOp (e, op) ->
      typecheck_expr ~ty_env e >>= fun e_ty -> typecheck_unop e_ty op
  | Identifier id -> typecheck_identifier id ty_env
  | Trigger -> typecheck_identifier (annotate ~loc trigger_id) ty_env
  | PropDeref (r, p) ->
      typecheck_expr ~ty_env r >>= fun r_ty -> typecheck_prop r_ty p
  | List lst -> typecheck_list lst ty_env
  | Range (s, e) ->
      typecheck_expr ~ty_env s
      >>= fun s_ty ->
      typecheck_expr ~ty_env e
      >>= fun e_ty ->
      if equal_types s_ty IntTy && equal_types e_ty IntTy then
        return (ListTy IntTy)
      else type_mismatch ~loc:expr.loc [IntTy; IntTy] [s_ty; e_ty]
  | Record fields -> typecheck_record fields ty_env
  | EventRef event_ref ->
      let event = !event_ref in
      typecheck_event event (ty_env, empty_env, label_types)
      >>= fun _ -> return (event_as_ty event)
  | _ ->
      fixme
        "forgot some expression type to validate in [typecheck_expr] function"
  )
  >>= fun value_ty ->
  ty := Some value_ty ;
  return value_ty

(* =============================================================================
   Type Equality functions
   ============================================================================= *)

and equal_types ty1 ty2 =
  let rec equal_types_aux = function
    | [] -> true
    | (ty1, ty2) :: rest -> (
      match (ty1, ty2) with
      | UnitTy, UnitTy | BoolTy, BoolTy | IntTy, IntTy | StringTy, StringTy ->
          (equal_types_aux [@tailcall]) rest
      | EventTy label1, EventTy label2 ->
          String.equal label1.data label2.data
          && (equal_types_aux [@tailcall]) rest
      | RecordTy fields1, RecordTy fields2 ->
          List.compare_lengths fields1 fields2 = 0
          &&
          let compare_by_name (name1, _) (name2, _) =
            String.compare name1.data name2.data
          in
          let sorted1 = List.sort compare_by_name fields1
          and sorted2 = List.sort compare_by_name fields2 in
          let combined = List.combine sorted1 sorted2 in
          List.for_all (fun (f1, f2) -> compare_by_name f1 f2 = 0) combined
          &&
          let type_pairs =
            List.map (fun ((_, v1), (_, v2)) -> (v1.data, v2.data)) combined
          in
          (equal_types_aux [@tailcall]) @@ type_pairs @ rest
      (* | ListTyEmpty, ListTyEmpty -> true *)
      | ListTy elem_type_1, ListTy elem_type_2 ->
          (equal_types_aux [@tailcall]) @@ ((elem_type_1, elem_type_2) :: rest)
      | _ -> false )
  in
  equal_types_aux [(ty1, ty2)]

(* =============================================================================
   Collect data dependency functions
   ============================================================================= *)

and _collect_event_dependencies event (_ty_env, event_env) =
  let {io; _} = event.data in
  match io.data with
  | Input _ ->
      (* TODO: When the Event types are added, go back to this function. *)
      return []
  | Output expr -> _collect_expr_dependencies expr (_ty_env, event_env)

and _collect_expr_dependencies expr (_ty_env, event_env) =
  (* Logger.debug @@ "Collecting dependencies for: " ;
     Logger.debug @@ unparse_expr expr ; *)
  let rec collect deps exprs =
    let collect_from_id id rest =
      match find_flat id.data event_env with
      | None -> id_not_found id
      | Some event -> collect (event :: deps) rest
    in
    (* print_endline "Current deps: ";
       print_endline (String.concat ", " (List.map (fun d -> unparse_events [d]) deps)); *)
    match exprs with
    | [] -> return deps
    | expr :: rest -> (
      match expr.data with
      | BinaryOp (l, r, _) -> collect deps (l :: r :: rest)
      | UnaryOp (e, _) -> collect deps (e :: rest)
      | Identifier id -> collect_from_id id rest
      | Trigger ->
          collect_from_id
            (annotate ~loc:expr.loc ~ty:!(expr.ty) trigger_id)
            rest
      | PropDeref (r, _) -> collect deps (r :: rest)
      | Record fields ->
          let values = List.map (fun (_, value) -> value) fields in
          collect deps (List.append values rest)
      | _ -> collect deps rest )
  in
  collect [] [expr]

(* =============================================================================
   Miscellaneous functions
   ============================================================================= *)

and default_value ty' =
  match ty' with
  | UnitTy -> Unit
  | BoolTy -> BoolLit false
  | IntTy -> IntLit 0
  | StringTy -> StringLit ""
  | RecordTy fields ->
      let default_field (name, ty) = (name, annotate (default_value ty.data)) in
      Record (List.map default_field fields)
  | _ -> failwith "Type not supported"
