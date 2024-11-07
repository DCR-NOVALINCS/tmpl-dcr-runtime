open Misc.Monads.ResultMonad
open Misc.Env
open Misc.Printing
open Syntax
open Errors
(* open Templating.Errors *)

(*
=============================================================================
  Typechecking types
=============================================================================
*)  

type typecheck_node = 
{ event: event
; type_dependencies: string list
; event_dependencies: string list
; uid_visible_event_env: string env
}

and typecheck_graph = (string * typecheck_node) list

and typecheck_context = 
{ uid_event_env: string env
; typecheck_graph: typecheck_graph
; type_env: type_expr env
; event_env: event env
}

(*
=============================================================================
  Aux functions
=============================================================================
*)  

(** {i (tail recursive)} [equal_types type_1 type_2] indicates whether type
    expressions [type_1] and [type_2] are structurally equal .

    Returns {b true} if the [type_1] and [type_2] are structurally equal, and
    {b false} otherwise. *)
let equal_types ty1 ty2 =
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
        (equal_types_aux [@tailcall]) @@ ((elem_type_1.data, elem_type_2.data) :: rest)
      | _ -> false)
  in
  equal_types_aux [ (ty1, ty2) ]

let empty_context = 
{ uid_event_env = empty_env
; typecheck_graph = []
; type_env = empty_env
; event_env = empty_env
}

let has_dependencies _ctxt = false

(*
=============================================================================
  Preprocessing functions
=============================================================================
*)  

let rec preprocess_subprogram ~ctxt ~events ~insts ~relations =
  Logger.group "Preprocessing subprogram...";
  preprocess_events ~ctxt events
  >>= fun ctxt ->
  preprocess_template_insts ~ctxt insts
  >>= fun ctxt ->
  preprocess_relations ~ctxt relations 
  >>| fun ctxt -> 
  Logger.end_group ();
  ctxt

and preprocess_events ~ctxt events = 
  Logger.debug "Preprocessing events...";
  let preprocess_event ctxt event = 
    let (id, _label) = event.data.info in
    let uid = fresh ~id_fn:(counter) id.data in
    Ok { ctxt with uid_event_env = (Misc.Env.bind id.data uid ctxt.uid_event_env) }
  in
  fold_left preprocess_event ctxt events

and preprocess_template_insts ~ctxt _template_insts = 
  Logger.debug "Preprocessing template instantiations...";
  Ok ctxt

and preprocess_relations ~ctxt _relations = 
  Logger.debug "Preprocessing relations...";
  Ok ctxt

(*
=============================================================================
 Typechecking functions
=============================================================================
*)  

let rec typecheck_template_defs _template_defs _env = Ok ()

and typecheck_subprogram ~events ~insts ~relations ~ctxt = 
  typecheck_events ~ctxt events 
  >>= fun ctxt ->
  typecheck_template_insts ~ctxt insts 
  >>= fun ctxt ->
  typecheck_relations ~ctxt relations

and typecheck_events ~ctxt events = 
  let typecheck_event ctxt _event = 
    Ok ctxt
  in
  fold_left typecheck_event ctxt events

and typecheck_template_insts ~ctxt _template_insts = Ok ctxt

and typecheck_relations ~ctxt _relations = Ok ctxt
    
(*
=============================================================================
  Main Typechecking functions
=============================================================================
*)  

let typecheck ?ctxt program = 
  (* Typecheck program:
    - Collect all labels and their corresponding types []
    - Collect event dependencies []
    - Check if there is any missing label []
    - Typecheck each template definition []
    - Typecheck each event []
    - Typecheck each template instantiation []
    - Typecheck each relation []
    - Repeat until there are no more dependencies left []
  *)
  Logger.info "Typechecking program...";
  let ctxt = match ctxt with
  | Some ctxt -> 
    ctxt
  | None -> 
    preprocess_subprogram 
    ~ctxt:empty_context
    ~events:program.events
    ~insts:program.template_insts
    ~relations:program.relations
  in
  Logger.group "Typechecking subprogram...";
  let rec typecheck_aux ~ctxt ~events ~insts ~relations = 
    typecheck_subprogram ~events ~insts ~relations ~ctxt
    >>= fun ctxt ->
    if has_dependencies ctxt 
      then typecheck_aux ~ctxt ~events ~insts ~relations
    else Ok ctxt
  in
  typecheck_aux ~ctxt ~events:program.events ~insts:program.template_insts ~relations:program.relations
  >>| fun ctxt ->
  Logger.end_group ();
  ctxt

let rec typecheck_expr ?(ty_env= empty_env) expr  = 
  let typecheck_binary_expr expr1 expr2 op ty_env = 
    typecheck_expr ~ty_env expr1
    >>= fun ty1 -> 
    typecheck_expr ~ty_env expr2
    >>= fun ty2 -> 
    begin match op with 
    | Add | Sub | Mult | Div -> Ok (IntTy, IntTy, IntTy)
    | Eq | NotEq -> Ok (ty1.data, ty1.data, BoolTy)
    | GreaterThan | GreaterOrEqual | LessThan | LessOrEqual -> Ok (IntTy, IntTy, BoolTy)
    | And | Or -> Ok (BoolTy, BoolTy, BoolTy)
    end
    >>= fun (expected_ty1, expected_ty2, result_ty) ->
    if not @@ equal_types ty1.data expected_ty1 
      then type_mismatch (annotate expected_ty1) [ty1]
    else if not @@ equal_types ty2.data expected_ty2 
      then type_mismatch (annotate expected_ty2) [ty2]
    else Ok (annotate ~loc:ty1.loc ~ty:(Some result_ty) result_ty) 
  in
  let typecheck_unary_expr expr op ty_env = 
    typecheck_expr ~ty_env expr
    >>= fun ty -> 
    begin match op with
    | Negation -> Ok (BoolTy, BoolTy)
    | Minus -> Ok (IntTy, IntTy)
    end 
    >>= fun (expected_ty, result_ty) ->
    if not @@ equal_types ty.data expected_ty 
      then type_mismatch (annotate expected_ty) [ty]
    else Ok (annotate ~loc:ty.loc ~ty:(Some result_ty) result_ty)
  in
  let typecheck_prop_deref expr prop ty_env = 
    typecheck_expr ~ty_env expr
    >>= fun ty -> 
    match ty.data with 
    | RecordTy fields -> 
      begin match List.assoc_opt prop fields with 
      | Some ty -> Ok ty
      | None -> id_not_found prop
      end
    | _ -> should_not_happen ~line:"137" ~module_path:"templating/typechecking.ml" "typecheck_prop_deref"
  in
  let typecheck_list es ty_env = 
    fold_left
      (fun expected_ty_opt e -> 
        typecheck_expr ~ty_env e
        >>= fun ty -> 
        match expected_ty_opt with
        | Some expected_ty -> 
          if ty.data = expected_ty.data then Ok (Some expected_ty)
          else type_mismatch expected_ty [ty]
        | None -> Ok (Some ty)
      )
      None es
    >>= function
    | Some ty -> Ok (annotate @@ ListTy ty)
    | None -> Ok (annotate @@ ListTy (annotate UnitTy)) (* Empty List *)
  in
  let typecheck_record fields ty_env = 
    map
      (fun (prop, e) -> 
        typecheck_expr ~ty_env e
        >>| fun ty -> (annotate prop.data, ty)
      )
      fields
    >>| fun fields -> (annotate (RecordTy fields))
  in
  let { data; loc; ty } = expr in
  begin match data with 
  | Unit -> Ok (annotate ~loc UnitTy)
  | True | False -> Ok (annotate ~loc BoolTy)
  | IntLit _ -> Ok (annotate ~loc IntTy)
  | StringLit _ -> Ok (annotate ~loc StringTy)
  | Parenthesized e -> typecheck_expr ~ty_env e 
  | BinaryOp (e1, e2, op) -> typecheck_binary_expr e1 e2 op ty_env
  | UnaryOp (e, op) -> typecheck_unary_expr e op ty_env
  | Identifier id -> 
    begin match find_flat id.data ty_env with 
    | Some ty -> Ok ty
    | None -> id_not_found id
    end
  | Trigger -> 
    begin match find_flat "@trigger" ty_env with
    | Some ty -> Ok ty
    | None -> id_not_found (annotate ~loc ~ty:(!ty) "@trigger")
    end
  | PropDeref (e, prop) -> typecheck_prop_deref e prop ty_env
  | List es -> typecheck_list es ty_env
  | Record fields -> typecheck_record fields ty_env
  | _ -> should_not_happen "typecheck_expr" 
  end >>| fun result_ty ->
  ty := Some result_ty.data;
  result_ty 