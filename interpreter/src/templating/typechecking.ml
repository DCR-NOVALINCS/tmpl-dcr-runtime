open Misc.Monads.ResultMonad
open Misc.Env
open Misc.Printing
open Syntax
open Errors
open Program_helper

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Aux functions, types and modules                                         │
   └──────────────────────────────────────────────────────────────────────────┘ *)

type event_kind = InputKind of type_expr' | OutputKind of type_expr'

type label_type = string * event_kind

module LabelTypeValue : Hashtbl.HashedType = struct
  type t = label_type

  let equal lt1 lt2 =
    let label1, kind1 = lt1 in
    let label2, kind2 = lt2 in
    String.equal label1 label2 && kind1 = kind2

  let hash lt = Hashtbl.hash lt
end

module LabelTypeHashtbl = Hashtbl.Make (LabelTypeValue)

type template_ty =
  { expr_param_tys: (string * type_expr') list
  ; event_param_tys: (string * label_type) list
  ; export_tys: label_type list }

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
  let _label_type_hashtbl = LabelTypeHashtbl.create 13 in
  typecheck_template_decls template_decls (ty_env, event_env)
  >>= fun (ty_env, event_env, tmpl_ty_env) ->
  typecheck_subprogram (events, insts, relations)
    (ty_env, event_env, tmpl_ty_env)

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

and typecheck_template_decl template_decl (ty_env, event_env, _tmpl_ty_env) =
  let {graph= events, insts, relations; export; export_types; params; _} =
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
  >>= fun (tmpl_ty_env, tmpl_event_env) ->
  (* Typecheck the parameters *)
  typecheck_params params (tmpl_ty_env, tmpl_event_env)
  >>= fun (tmpl_ty_env, tmpl_event_env) ->
  (* Typecheck the graph of the template *)
  typecheck_subprogram (events, insts, relations)
    (tmpl_ty_env, tmpl_event_env, tmpl_ty_env)
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
    >>= fun _ -> return (ty_env, event_env, tmpl_ty_env)

(* =============================================================================
   Typechecking of subprograms
   ============================================================================= *)

and typecheck_subprogram (events, insts, relations)
    (ty_env, event_env, tmpl_ty_env) =
  typecheck_events events (ty_env, event_env)
  >>= fun (ty_env, event_env) ->
  Logger.debug @@ "Event env: " ;
  Logger.debug
  @@ string_of_env
       (fun e -> Unparser.PlainUnparser.unparse_events [e])
       event_env ;
  typecheck_insts insts (ty_env, event_env, tmpl_ty_env)
  >>= fun (ty_env, event_env) ->
  typecheck_relations relations (ty_env, event_env, tmpl_ty_env)

(* =============================================================================
   Typechecking of events
   ============================================================================= *)

and typecheck_events events (ty_env, event_env) =
  fold_left
    (fun (ty_env, event_env) event -> typecheck_event event (ty_env, event_env))
    (ty_env, event_env) events

and typecheck_event event (ty_env, event_env) =
  let {info; io; _} = event.data in
  let id, _ = info in
  ( match io.data with
  | Input ty -> return ty.data
  | Output expr -> typecheck_expr ~ty_env expr >>= fun ty -> return ty )
  >>= fun ty ->
  return (RecordTy [(annotate "value", annotate ty)])
  (* return (event_as_ty event) *)
  >>= fun event_ty ->
  io.ty := Some event_ty ;
  return (bind id.data event_ty ty_env, bind id.data event event_env)

(* =============================================================================
   Typechecking of template instances
   ============================================================================= *)

and typecheck_insts insts (ty_env, event_env, tmpl_ty_env) =
  fold_left
    (fun (ty_env, event_env) inst ->
      typecheck_inst inst (ty_env, event_env, tmpl_ty_env) )
    (ty_env, event_env) insts

and typecheck_inst inst (ty_env, event_env, _tmpl_ty_env) =
  let {args; x; _} = inst.data in
  let rec typecheck_args args (ty_env, event_env) =
    fold_left
      (fun (ty_env, event_env) (_id, arg_type) ->
        typecheck_arg arg_type (ty_env, event_env) )
      (ty_env, event_env) args
  and typecheck_arg arg (ty_env, event_env) =
    match arg with
    | ExprArg expr ->
        typecheck_expr ~ty_env expr >>= fun _ -> return (ty_env, event_env)
    | EventArg event_id -> (
      match find_flat event_id.data event_env with
      | None -> id_not_found event_id
      | Some _event -> return (ty_env, event_env) )
  and make_x_events xs (ty_env, event_env) =
    (* Binds exported events to be used in the latter *)
    fold_left
      (fun (ty_env, event_env) x ->
        return @@ mk_event (x, annotate "X") (annotate (Input (annotate IntTy)))
        >>= fun x_event -> typecheck_event x_event (ty_env, event_env) )
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
      typecheck_subprogram subprogram (ty_env, event_env, tmpl_ty_env)

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
    | Minus -> return (BoolTy, BoolTy)
    | Negation -> return (IntTy, IntTy) )
    >>= fun (expected_e_ty, expected_result_ty) ->
    if e_ty = expected_e_ty then return expected_result_ty
    else type_mismatch ~loc:expr.loc [expected_e_ty; expected_result_ty] [e_ty]
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
