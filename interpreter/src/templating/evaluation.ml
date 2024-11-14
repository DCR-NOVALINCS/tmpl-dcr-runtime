open Misc.Env
open Misc.Monads.ResultMonad
open Syntax
open Errors

(* =============================================================================
   Expression Evaluation functions
   ============================================================================= *)

let rec eval_expr expr env =
  match expr.data with
  | Unit ->
      expr.ty := Some UnitTy ;
      return {expr with ty= ref (Some UnitTy)}
  | True | False ->
      expr.ty := Some BoolTy ;
      return {expr with ty= ref (Some BoolTy)}
  | IntLit _ ->
      expr.ty := Some IntTy ;
      return {expr with ty= ref (Some IntTy)}
  | StringLit _ ->
      expr.ty := Some StringTy ;
      return {expr with ty= ref (Some StringTy)}
  | Parenthesized e -> eval_expr e env
  | BinaryOp (e1, e2, op) ->
      eval_expr e1 env
      >>= fun v1 -> eval_expr e2 env >>= fun v2 -> eval_binop v1 v2 op
  | UnaryOp (e, op) -> eval_expr e env >>= fun v -> eval_unop v op
  | Identifier id -> find_id id env
  | Trigger -> find_id (annotate ~loc:expr.loc ~ty:!(expr.ty) "@trigger") env
  | PropDeref (e, p) -> (
      eval_expr e env
      >>= fun v ->
      let rec_ty = Option.value ~default:UnitTy !(v.ty) in
      match v.data with
      | Record fields -> (
          let fields =
            List.map (fun (name, expr) -> (name.data, expr)) fields
          in
          match List.assoc_opt p.data fields with
          | None -> property_not_found p v
          | Some v -> return v )
      | _ -> type_mismatch [RecordTy [(p, annotate UnitTy)]] [rec_ty] )
  | List es ->
      map (fun e -> eval_expr e env) es >>| fun es -> {expr with data= List es}
  | Record fields ->
      map
        (fun (name, expr) -> eval_expr expr env >>= fun v -> return (name, v))
        fields
      >>= fun expr_fields ->
      map
        (fun (name, expr) ->
          match !(expr.ty) with
          | Some ty -> return (name, annotate ~loc:expr.loc ~ty:(Some ty) ty)
          | _ ->
              should_not_happen ~module_path:"evaluation.ml" ~line:"58"
                "The type of the record field is not annotated" )
        expr_fields
      >>| fun type_fields ->
      expr.ty := Some (RecordTy type_fields) ;
      {expr with data= Record expr_fields; ty= ref (Some (RecordTy type_fields))}
  | _ -> invalid_expr ()

and eval_binop v1 v2 op =
  ( match (!(v1.ty), !(v2.ty)) with
  | Some ty1, Some ty2 -> return (ty1, ty2)
  (* | Some ty1, _ -> return (ty1, UnitTy) | _, Some ty2 -> return (UnitTy,
     ty2) *)
  | Some ty1, None ->
      should_not_happen ~module_path:"evaluation.ml"
        (Printf.sprintf
           "The type of the second operand is missing, the first operand is %s"
           (Unparser.PlainUnparser.unparse_ty ty1) )
  | None, Some ty2 ->
      should_not_happen ~module_path:"evaluation.ml"
        (Printf.sprintf
           "The type of the first operand is missing, the second operand is %s"
           (Unparser.PlainUnparser.unparse_ty ty2) )
  | _ ->
      should_not_happen
        "Either the types of the first or second operand are missing" )
  >>= fun (ty1, ty2) ->
  match op with
  | Add -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        return {v1 with data= IntLit (i1 + i2); ty= ref (Some IntTy)}
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | Sub -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        return {v1 with data= IntLit (i1 - i2); ty= ref (Some IntTy)}
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | Mult -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        return {v1 with data= IntLit (i1 * i2); ty= ref (Some IntTy)}
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | Div -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        return {v1 with data= IntLit (i1 / i2); ty= ref (Some IntTy)}
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | Eq -> (
    match (v1.data, v2.data) with
    (* Number *)
    | IntLit i1, IntLit i2 ->
        if i1 = i2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | IntLit _, _ | _, IntLit _ -> type_mismatch [IntTy] [ty1; ty2]
    (* String *)
    | StringLit s1, StringLit s2 ->
        if s1 = s2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | StringLit _, _ | _, StringLit _ -> type_mismatch [StringTy] [ty1; ty2]
    (* Boolean *)
    | True, True | False, False ->
        return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | True, False | False, True ->
        return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | True, _ | False, _ | _, True | _, False ->
        type_mismatch [BoolTy] [ty1; ty2]
    (* Other *)
    | _ -> type_mismatch [IntTy; StringTy; BoolTy] [ty1; ty2] )
  | NotEq -> (
    match (v1.data, v2.data) with
    (* Number *)
    | IntLit i1, IntLit i2 ->
        if i1 <> i2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | IntLit _, _ | _, IntLit _ -> type_mismatch [IntTy] [ty1; ty2]
    (* String *)
    | StringLit s1, StringLit s2 ->
        if s1 <> s2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | StringLit _, _ | _, StringLit _ -> type_mismatch [StringTy] [ty1; ty2]
    (* Boolean *)
    | True, False | False, True ->
        return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | True, True | False, False ->
        return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | True, _ | False, _ | _, True | _, False ->
        type_mismatch [BoolTy] [ty1; ty2]
    (* Other *)
    | _ -> type_mismatch [IntTy; StringTy] [ty1; ty2] )
  | GreaterThan -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        if i1 > i2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | IntLit _, _ | _, IntLit _ -> type_mismatch [IntTy] [ty1; ty2]
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | GreaterOrEqual -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        if i1 >= i2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | IntLit _, _ | _, IntLit _ -> type_mismatch [IntTy] [ty1; ty2]
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | LessThan -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        if i1 < i2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | IntLit _, _ | _, IntLit _ -> type_mismatch [IntTy] [ty1; ty2]
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | LessOrEqual -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        if i1 <= i2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | IntLit _, _ | _, IntLit _ -> type_mismatch [IntTy] [ty1; ty2]
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | And -> (
    match (v1.data, v2.data) with
    | True, True -> return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | True, False | False, True | False, False ->
        return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | _ -> type_mismatch [BoolTy] [ty1; ty2] )
  | Or -> (
    match (v1.data, v2.data) with
    | False, False -> return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | True, False | False, True | True, True ->
        return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | _ -> type_mismatch [BoolTy] [ty1; ty2] )

and eval_unop v op =
  (* let ty = Option.value ~default:UnitTy !(v.ty) in *)
  ( match !(v.ty) with
  | Some ty -> return ty
  | None ->
      should_not_happen ~module_path:"evaluation.ml"
        (Printf.sprintf "The type of the operand is missing, the operand is %s"
           (Unparser.PlainUnparser.unparse_expr v) ) )
  >>= fun ty ->
  match op with
  | Minus -> (
    match v.data with
    | IntLit i -> return {v with data= IntLit (-i); ty= ref (Some IntTy)}
    | _ -> type_mismatch [IntTy] [ty] )
  | Negation -> (
    match v.data with
    | True -> return (annotate ~loc:v.loc ~ty:(Some BoolTy) False)
    | False -> return (annotate ~loc:v.loc ~ty:(Some BoolTy) True)
    | _ -> type_mismatch [BoolTy] [ty] )
(* | _ -> failwith "Invalid unary operator" *)

and find_id id env =
  match find_flat id.data env with
  | None -> id_not_found id
  | Some expr -> return expr

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
          (equal_types_aux [@tailcall])
          @@ ((elem_type_1.data, elem_type_2.data) :: rest)
      | _ -> false )
  in
  equal_types_aux [(ty1, ty2)]

(* =============================================================================
   Expression Partial Evaluation functions
   ============================================================================= *)

(* See more: https://link.springer.com/chapter/10.1007/3-540-11980-9_13 *)
and partial_eval_expr expr expr_env =
  match expr.data with
  | Parenthesized e ->
      partial_eval_expr e expr_env
      >>= fun v -> return {expr with data= Parenthesized v}
  | BinaryOp (e1, e2, op) ->
      partial_eval_expr e1 expr_env
      >>= fun v1 ->
      partial_eval_expr e2 expr_env
      >>= fun v2 -> return {expr with data= BinaryOp (v1, v2, op)}
  | UnaryOp (e, op) ->
      partial_eval_expr e expr_env
      >>= fun v -> return {expr with data= UnaryOp (v, op)}
  | Identifier id -> find_id id expr_env
  (* TODO: Put more cases *)
  | _ -> return expr
