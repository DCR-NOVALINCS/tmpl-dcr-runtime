open Misc.Env
open Misc.Monads.ResultMonad
open Syntax
open Errors

(* =============================================================================
   Entry point
   ============================================================================= *)

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
let rec typecheck ?(expr_env = empty_env) ?(event_env = empty_env) program =
  (* todo "typecheck template defs" >>= fun _ -> *)
  let events = program.events in
  typecheck_events events (expr_env, event_env)
  (* todo "typecheck events" *)
  >>= fun _ ->
  todo "typecheck insts"
  >>= fun _ -> todo "typecheck relations" >>= fun _ -> return true

(* =============================================================================
   Typechecking of events
   ============================================================================= *)

and typecheck_events events (expr_env, event_env) =
  fold_left
    (fun (expr_env, event_env) event ->
      typecheck_event event (expr_env, event_env) )
    (expr_env, event_env) events

and typecheck_event event (expr_env, event_env) =
  let {info; _} = event.data in
  let id, _ = info in
  let open Misc.Env in
  (* Bind the event *)
  return (bind id.data event event_env)
  >>= fun event_env -> return (expr_env, event_env)

(* =============================================================================
   Typechecking of expressions
   ============================================================================= *)

and typecheck_expr expr expr_env =
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
    else
      type_mismatch
        [expected_l_ty; expected_r_ty; expected_result_ty]
        [l_ty; r_ty]
  and typecheck_unop e_ty op =
    ( match op with
    | Minus -> return (BoolTy, BoolTy)
    | Negation -> return (IntTy, IntTy) )
    >>= fun (expected_e_ty, expected_result_ty) ->
    if e_ty = expected_e_ty then return expected_result_ty
    else type_mismatch [expected_e_ty; expected_result_ty] [e_ty]
  and typecheck_identifier id expr_env =
    match find_flat id.data expr_env with
    | None -> id_not_found id
    | Some ty -> return ty
  and typecheck_prop r_ty p =
    match r_ty with
    | RecordTy fields -> (
        let fields = List.map (fun (p, ty) -> (deannotate p, ty)) fields in
        match List.assoc_opt p.data fields with
        | Some ty -> return ty.data
        | None -> property_not_found_type p r_ty )
    | _ -> type_mismatch [RecordTy []] [r_ty]
  and typecheck_list _lst _expr_env = todo "typecheck list"
  and typecheck_record _fields _expr_env = todo "typecheck record" in
  let {data; ty; loc} = expr in
  ( match data with
  | Unit -> return UnitTy
  | True | False -> return BoolTy
  | IntLit _ -> return IntTy
  | StringLit _ -> return StringTy
  | Parenthesized e -> typecheck_expr e expr_env
  | BinaryOp (l, r, op) ->
      typecheck_expr l expr_env
      >>= fun l_ty ->
      typecheck_expr r expr_env >>= fun r_ty -> typecheck_binop l_ty r_ty op
  | UnaryOp (e, op) ->
      typecheck_expr e expr_env >>= fun e_ty -> typecheck_unop e_ty op
  | Identifier id -> typecheck_identifier id expr_env
  | Trigger -> typecheck_identifier (annotate ~loc "@trigger") expr_env
  | PropDeref (r, p) ->
      typecheck_expr r expr_env >>= fun r_ty -> typecheck_prop r_ty p
  | List lst -> typecheck_list lst expr_env
  | Range (s, e) ->
      typecheck_expr s expr_env
      >>= fun s_ty ->
      typecheck_expr e expr_env
      >>= fun e_ty ->
      if equal_types s_ty IntTy && equal_types e_ty IntTy then
        return (ListTy IntTy)
      else type_mismatch [IntTy; IntTy] [s_ty; e_ty]
  | Record fields -> typecheck_record fields expr_env
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
