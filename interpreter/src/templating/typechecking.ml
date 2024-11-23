open Misc.Monads.ResultMonad
open Misc.Env
open Misc.Printing
open Syntax
open Errors
open Program_helper

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
  let events = program.events in
  let insts = program.template_insts in
  let relations = program.relations in
  let ty_env = empty_env in
  typecheck_subprogram (events, insts, relations) (ty_env, event_env)

(* =============================================================================
   Typechecking of subprograms
   ============================================================================= *)

and typecheck_subprogram (events, insts, relations) (ty_env, event_env) =
  typecheck_events events (ty_env, event_env)
  >>= fun (ty_env, event_env) ->
  typecheck_insts insts (ty_env, event_env)
  >>= fun (ty_env, event_env) ->
  typecheck_relations relations (ty_env, event_env)

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
  return (bind id.data event_ty ty_env)
  >>= fun ty_env -> return (ty_env, event_env)

(* =============================================================================
   Typechecking of template instances
   ============================================================================= *)

and typecheck_insts insts (ty_env, event_env) =
  fold_left
    (fun (ty_env, event_env) inst -> typecheck_inst inst (ty_env, event_env))
    (ty_env, event_env) insts

and typecheck_inst _inst (_ty_env, _event_env) = todo "typecheck inst"

(* =============================================================================
   Typechecking of relations
   ============================================================================= *)

and typecheck_relations relations (ty_env, event_env) =
  fold_left
    (fun (ty_env, event_env) relation ->
      typecheck_relation relation (ty_env, event_env) )
    (ty_env, event_env) relations

and typecheck_relation relation (ty_env, event_env) =
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
    else type_mismatch [BoolTy] [guard_ty]
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
      typecheck_subprogram subprogram (ty_env, event_env)

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
        else type_mismatch [l_ty; r_ty; BoolTy] [l_ty; r_ty]
    | GreaterThan | GreaterOrEqual | LessThan | LessOrEqual ->
        return (IntTy, IntTy, BoolTy) )
    >>= fun (expected_l_ty, expected_r_ty, expected_result_ty) ->
    if equal_types expected_l_ty l_ty && equal_types expected_r_ty r_ty then
      return expected_result_ty
    else type_mismatch [expected_l_ty; expected_r_ty] [l_ty; r_ty]
  (* Typechecking a unary operation, e.g -, ~, etc. *)
  and typecheck_unop e_ty op =
    ( match op with
    | Minus -> return (BoolTy, BoolTy)
    | Negation -> return (IntTy, IntTy) )
    >>= fun (expected_e_ty, expected_result_ty) ->
    if e_ty = expected_e_ty then return expected_result_ty
    else type_mismatch [expected_e_ty; expected_result_ty] [e_ty]
  (* Typechecking a identifier *)
  and typecheck_identifier id ty_env =
    match find_flat id.data ty_env with
    | None ->
        Logger.debug @@ "Type env:\n"
        ^ string_of_env Unparser.PlainUnparser.unparse_ty ty_env ;
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
    | _ -> type_mismatch [RecordTy []] [r_ty]
  (* Typechecking a list *)
  and typecheck_list lst ty_env =
    (* Typechecking each element of the list [lst] *)
    map (fun e -> typecheck_expr ~ty_env e) lst
    >>= fun elem_tys ->
    let expected_ty = List.hd elem_tys in
    partition_map
      (fun elem_ty ->
        if equal_types elem_ty expected_ty then Either.left elem_ty
        else Either.right elem_ty )
      elem_tys
    >>= fun (_, wrong_tys) ->
    if List.is_empty wrong_tys then return (ListTy expected_ty)
    else type_mismatch [expected_ty] wrong_tys
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
      else type_mismatch [IntTy; IntTy] [s_ty; e_ty]
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
