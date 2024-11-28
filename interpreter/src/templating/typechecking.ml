open Misc.Monads.ResultMonad
open Misc.Env
open Misc.Printing
open Syntax
open Errors
open Program_helper

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Aux functions, types and modules                                         │
   └──────────────────────────────────────────────────────────────────────────┘ *)

type event_kind = type_expr' * event_type'

module EventTypes = struct
  (* type label_type = {label: string; kind: event_kind}

     let mk_label_type label kind = {label; kind} *)

  module LabelTypeHashtbl = Hashtbl.Make (String)

  let size = 13

  let empty : event_kind LabelTypeHashtbl.t = LabelTypeHashtbl.create size

  let add (label, kind) tbl =
    LabelTypeHashtbl.replace tbl label kind ;
    tbl

  let find label tbl = LabelTypeHashtbl.find_opt tbl label

  let remove label tbl =
    LabelTypeHashtbl.remove tbl label ;
    tbl

  let show tbl =
    let f key value acc =
      let kind =
        let value_ty, event_type = value in
        let value_ty_str = Unparser.PlainUnparser.unparse_ty value_ty in
        match event_type with
        | InputType -> "Input(" ^ value_ty_str ^ ")"
        | OutputType -> "Output(" ^ value_ty_str ^ ")"
      in
      acc ^ key ^ ": " ^ kind ^ "\n"
    in
    LabelTypeHashtbl.fold f tbl ""
end

type template_ty =
  { expr_param_tys: (string * type_expr') list
  ; event_param_labels: (string * string) list
  ; export_tys: (string * string) list }

let mk_template_ty ?(expr_param_tys = []) ?(event_param_labels = [])
    ?(export_tys = []) () =
  {expr_param_tys; event_param_labels; export_tys}

let mk_template_ty_from template_def =
  let {params; export; export_types; _} = template_def in
  let expr_param_tys =
    List.filter_map
      (fun (id, ty) ->
        match ty with ExprParam (ty, _) -> Some (id.data, ty.data) | _ -> None
        )
      params
  in
  let event_param_labels =
    List.filter_map
      (fun (id, ty) ->
        match ty with
        | EventParam label -> Some (id.data, label.data)
        | _ -> None )
      params
  in
  let export_tys =
    List.combine (deannotate_list export) (deannotate_list export_types)
  in
  mk_template_ty ~expr_param_tys ~event_param_labels ~export_tys ()

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Entry point                                                              │
   └──────────────────────────────────────────────────────────────────────────┘ *)

(* Typecheck the program *)
(* - [] Check the template definitions\ *)

(* - Check the events *)
(* - [] Check the expr inside of the event's marking and value *)
(* - [] Check the instances *)

(* - Check the relations *)
(* - [] Check the [from] and [to] events *)
(* - [] Check the [guard] *)

(* - If the relation is a [spawn] relation: *)
(* - [] *)
let rec typecheck ?(event_env = empty_env) program =
  (* todo "typecheck template defs" >>= fun _ -> *)
  let template_decls = program.template_decls in
  let events = program.events in
  let insts = program.template_insts in
  let relations = program.relations in
  let ty_env = empty_env in
  let label_types = EventTypes.empty in
  typecheck_template_decls template_decls (ty_env, event_env)
  >>= fun (ty_env, event_env, tmpl_ty_env) ->
  typecheck_subprogram (events, insts, relations)
    (ty_env, event_env, tmpl_ty_env, label_types)

(* =============================================================================
   Typechecking of template definitions
   ============================================================================= *)

and typecheck_template_decls template_decls ?(tmpl_ty_env = empty_env)
    (ty_env, event_env) =
  fold_left
    (fun (ty_env, event_env, tmpl_ty_env) template_decl ->
      typecheck_template_decl template_decl (ty_env, event_env, tmpl_ty_env) )
    (ty_env, event_env, tmpl_ty_env)
    template_decls

and typecheck_template_decl template_decl (ty_env, event_env, tmpl_ty_env) =
  let {graph= events, insts, relations; export; export_types; params; id; _} =
    template_decl
  and typecheck_params params (ty_env, event_env) =
    fold_left
      (fun (ty_env, event_env) (id, param_type) ->
        match param_type with
        | ExprParam (ty, _default) ->
            Logger.debug "Binding expr: " ;
            Logger.debug @@ Unparser.PlainUnparser.unparse_ty ty.data ;
            return (bind id.data ty.data ty_env)
            >>= fun ty_env -> return (ty_env, event_env)
        | EventParam label ->
            Logger.debug "Binding event: " ;
            Logger.debug label.data ;
            (* FIXME: need to change the value of the event passed as param. *)
            (* TODO: Get the kind of the label, e.g
                - (a: A)[?: Number]
                - (b: B)[{x: Number}]
                Labels:
                - A -> Input(Number)
                - B -> Output(RecordTy[{x: Number}])
            *)
            let event =
              mk_event (id, label) (annotate (Input (annotate UnitTy)))
            in
            return (bind id.data event event_env)
            >>= fun event_env -> return (ty_env, event_env) )
      (ty_env, event_env) params
  in
  (* Begin new scope *)
  return @@ (begin_scope ty_env, begin_scope event_env)
  >>= fun (ty_env, event_env) ->
  (* Typecheck the parameters *)
  typecheck_params params (ty_env, event_env)
  >>= fun (ty_env, event_env) ->
  (* Typecheck the graph of the template *)
  typecheck_subprogram (events, insts, relations)
    (ty_env, event_env, tmpl_ty_env, EventTypes.empty)
  >>= fun (_tmpl_ty_env, tmpl_event_env) ->
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
  else if not (List.length exported_events = List.length export) then
    (* Error when the number of exported events does not match the number of export types *)
    todo "error message for excess or less of exported events"
  else if not (List.length exported_events = List.length export_types) then
    todo "error message for excess or less of exported events"
  else
    return @@ List.combine exported_events export_types
    >>= fun exported_type_mapping ->
    iter
      (fun (event, label_ty) ->
        let {info= _, label; _} = event.data in
        if label.data = label_ty.data then return ()
        else
          type_mismatch ~loc:label_ty.loc [EventTy label_ty.data]
            [EventTy label.data] )
      exported_type_mapping
    >>= fun _ ->
    return (mk_template_ty_from template_decl)
    >>= fun tmpl_ty ->
    return (bind id.data tmpl_ty tmpl_ty_env)
    >>= fun tmpl_ty_env ->
    return (end_scope ty_env, end_scope event_env, tmpl_ty_env)

(* =============================================================================
   Typechecking of subprograms
   ============================================================================= *)

and typecheck_subprogram (events, insts, relations)
    (ty_env, event_env, tmpl_ty_env, label_types) =
  typecheck_events events (ty_env, event_env, label_types)
  >>= fun (ty_env, event_env, label_types) ->
  Logger.debug @@ "Event env: " ;
  Logger.debug
  @@ string_of_env
       (fun e -> Unparser.PlainUnparser.unparse_events [e])
       event_env ;
  Logger.debug @@ "Label type hashtbl: " ;
  Logger.debug @@ EventTypes.show label_types ;
  typecheck_insts insts (ty_env, event_env, tmpl_ty_env)
  >>= fun (ty_env, event_env) ->
  typecheck_relations relations (ty_env, event_env, tmpl_ty_env)

(* =============================================================================
   Typechecking of events
   ============================================================================= *)

and typecheck_events events (ty_env, event_env, label_types) =
  fold_left
    (fun (ty_env, event_env, label_types) event ->
      typecheck_event event (ty_env, event_env, label_types) )
    (ty_env, event_env, label_types)
    events

and typecheck_event event (ty_env, event_env, label_types) =
  (* collect_event_dependencies event (ty_env, event_env)
     >>= fun deps ->
     Logger.debug @@ "Dependencies: " ;
     Logger.debug
     @@ String.concat ", "
          (List.map (fun d -> Unparser.PlainUnparser.unparse_events [d]) deps) ; *)
  let {info; io; _} = event.data in
  let id, label = info in
  ( match io.data with
  | Input ty -> return (ty.data, InputType)
  | Output expr ->
      typecheck_expr ~ty_env expr >>= fun ty -> return (ty, OutputType) )
  >>= fun (got_value_ty, got_event_type) ->
  ( match EventTypes.find label.data label_types with
  | None ->
      return
        (EventTypes.add
           (label.data, (got_value_ty, got_event_type))
           label_types )
  | Some (expected_value_ty, expected_event_type) ->
      if
        got_value_ty = expected_value_ty && got_event_type = expected_event_type
      then return label_types
      else
        event_type_mismatch ~loc:io.loc
          [(label.data, expected_event_type, expected_value_ty)]
          [(label.data, got_event_type, got_value_ty)] )
  >>= fun label_types ->
  return (RecordTy [(annotate "value", annotate got_value_ty)])
  (* return (event_as_ty event) *)
  >>= fun event_ty ->
  io.ty := Some event_ty ;
  return
    (bind id.data event_ty ty_env, bind id.data event event_env, label_types)

(* =============================================================================
   Typechecking of template instances
   ============================================================================= *)

and typecheck_insts insts (ty_env, event_env, tmpl_ty_env) =
  fold_left
    (fun (ty_env, event_env) inst ->
      typecheck_inst inst (ty_env, event_env, tmpl_ty_env) )
    (ty_env, event_env) insts

and typecheck_inst inst (ty_env, event_env, tmpl_ty_env) =
  let {args; x; tmpl_id; _} = inst.data in
  (* Find the template used *)
  match find_flat tmpl_id.data tmpl_ty_env with
  | None -> tmpl_not_found tmpl_id
  | Some template_ty ->
      let {expr_param_tys= _; event_param_labels= _; export_tys= _} =
        template_ty
      in
      let typecheck_args args (ty_env, event_env) =
        fold_left
          (fun (ty_env, event_env) (_id, arg_type) ->
            match arg_type with
            | ExprArg expr ->
                typecheck_expr ~ty_env expr
                >>= fun _ -> return (ty_env, event_env)
            | EventArg event_id -> (
              match find_flat event_id.data event_env with
              | None -> id_not_found event_id
              | Some _event -> return (ty_env, event_env) ) )
          (ty_env, event_env) args
      and make_x_events xs (ty_env, event_env) =
        (* Binds exported events to be used in the latter *)
        fold_left
          (fun (ty_env, event_env) x ->
            return
            @@ mk_event (x, annotate "X") (annotate (Input (annotate IntTy)))
            >>= fun x_event ->
            typecheck_event x_event (ty_env, event_env, EventTypes.empty)
            >>= fun (ty_env, event_env, _) -> return (ty_env, event_env) )
          (ty_env, event_env) xs
      in
      typecheck_args args (ty_env, event_env)
      >>= fun (ty_env, event_env) ->
      make_x_events x (ty_env, event_env)
      >>= fun (ty_env, event_env) -> return (ty_env, event_env)

(* =============================================================================
   Typechecking of relations
   ============================================================================= *)

and typecheck_relations relations (ty_env, event_env, tmpl_ty_env) =
  fold_left
    (fun (ty_env, event_env) relation ->
      typecheck_relation relation (ty_env, event_env, tmpl_ty_env) )
    (ty_env, event_env) relations

and typecheck_relation relation (ty_env, event_env, tmpl_ty_env) =
  let check_event_id id =
    match find_flat id.data event_env with
    | None ->
        Logger.debug @@ "Event env: "
        ^ string_of_env
            (fun e -> Unparser.PlainUnparser.unparse_events [e])
            event_env ;
        id_not_found id
    | Some event -> return event
  and check_guard_expr guard =
    typecheck_expr ~ty_env guard
    >>= fun guard_ty ->
    if equal_types guard_ty BoolTy then return BoolTy
    else type_mismatch ~loc:guard.loc [BoolTy] [guard_ty]
  in
  match relation.data with
  | ControlRelation (from_id, guard, dest, _op, _annot) ->
      check_event_id from_id
      >>= fun _ ->
      check_event_id dest
      >>= fun _ -> check_guard_expr guard >>= fun _ -> return (ty_env, event_env)
  | SpawnRelation (from_id, guard, subprogram, _annot) ->
      check_event_id from_id
      >>= fun from_event ->
      check_guard_expr guard
      >>= fun _ ->
      return (begin_scope event_env, begin_scope ty_env)
      >>= fun (event_env, ty_env) ->
      return
        ( bind "@trigger" from_event event_env
        , bind "@trigger" (event_as_ty from_event) ty_env )
      >>= fun (event_env, ty_env) ->
      typecheck_subprogram subprogram
        (ty_env, event_env, tmpl_ty_env, EventTypes.empty)

(* =============================================================================
   Typechecking of expressions
   ============================================================================= *)

and typecheck_expr ?(ty_env = empty_env) expr =
  (* Typechecking a binary operation, e.g +, *, etc. *)
  let typecheck_binop l_ty r_ty op =
    ( match op with
    | Add | Sub | Mult | Div -> return (IntTy, IntTy, IntTy)
    | And | Or -> return (BoolTy, BoolTy, BoolTy)
    | Eq | NotEq ->
        if equal_types l_ty r_ty then return (l_ty, r_ty, BoolTy)
        else type_mismatch ~loc:expr.loc [l_ty; r_ty; BoolTy] [l_ty; r_ty]
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
           ^ string_of_env Unparser.PlainUnparser.unparse_ty ty_env ; *)
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
    | _ -> type_mismatch ~loc:expr.loc [RecordTy []] [r_ty]
  (* Typechecking a list *)
  and typecheck_list lst ty_env =
    (* Typechecking each element of the list [lst] *)
    map (fun e -> typecheck_expr ~ty_env e) lst
    >>= fun elem_tys ->
    let expected_ty = List.hd elem_tys in
    partition_map
      (fun elem_ty ->
        if equal_types elem_ty expected_ty then Left elem_ty else Right elem_ty
        )
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
  | True | False -> return BoolTy
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
  | Trigger -> typecheck_identifier (annotate ~loc "@trigger") ty_env
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

(** {i (tail recursive)} [equal_types type_1 type_2] indicates whether type
    expressions [type_1] and [type_2] are structurally equal .

    Returns {b true} if the [type_1] and [type_2] are structurally equal, and
    {b false} otherwise. *)
and equal_types ty1 ty2 =
  let rec equal_types_aux = function
    | [] -> true
    | (ty1, ty2) :: rest -> (
      match (ty1, ty2) with
      | UnitTy, UnitTy | BoolTy, BoolTy | IntTy, IntTy | StringTy, StringTy ->
          (equal_types_aux [@tailcall]) rest
      | EventTy label1, EventTy label2 ->
          String.equal label1 label2 && (equal_types_aux [@tailcall]) rest
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

and collect_event_dependencies event (_ty_env, event_env) =
  let {io; _} = event.data in
  match io.data with
  | Input _ ->
      (* TODO: When the Event types are added, go back to this function. *)
      return []
  | Output expr -> collect_expr_dependencies expr (_ty_env, event_env)

and collect_expr_dependencies expr (_ty_env, event_env) =
  (* Logger.debug @@ "Collecting dependencies for: " ;
     Logger.debug @@ Unparser.PlainUnparser.unparse_expr expr ; *)
  let rec collect deps exprs =
    let collect_from_id id rest =
      match find_flat id.data event_env with
      | None -> id_not_found id
      | Some event -> collect (event :: deps) rest
    in
    (* print_endline "Current deps: ";
       print_endline (String.concat ", " (List.map (fun d -> Unparser.PlainUnparser.unparse_events [d]) deps)); *)
    match exprs with
    | [] -> return deps
    | expr :: rest -> (
      match expr.data with
      | BinaryOp (l, r, _) -> collect deps (l :: r :: rest)
      | UnaryOp (e, _) -> collect deps (e :: rest)
      | Identifier id -> collect_from_id id rest
      | Trigger ->
          collect_from_id
            (annotate ~loc:expr.loc ~ty:!(expr.ty) "@trigger")
            rest
      | PropDeref (r, _) -> collect deps (r :: rest)
      | Record fields ->
          let values = List.map (fun (_, value) -> value) fields in
          collect deps (List.append values rest)
      | _ -> collect deps rest )
  in
  collect [] [expr]
