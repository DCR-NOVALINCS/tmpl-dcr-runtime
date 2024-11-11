open Misc.Env
open Misc.Monads.ResultMonad
open Syntax
open Errors

(* =============================================================================
   Expression Evaluation functions
   ============================================================================= *)

let rec eval_expr expr env =
  match expr.data with
  | Unit -> Ok {expr with ty= ref (Some UnitTy)}
  | True | False -> Ok {expr with ty= ref (Some BoolTy)}
  | IntLit _ -> Ok {expr with ty= ref (Some IntTy)}
  | StringLit _ -> Ok {expr with ty= ref (Some StringTy)}
  | Reference reference -> eval_expr !reference env
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
            List.map (fun (prop, expr) -> (prop.data, expr)) fields
          in
          match List.assoc_opt p.data fields with
          | None -> property_not_found p v
          | Some v -> Ok (annotate ~loc:p.loc ~ty:!(v.ty) v.data) )
      | _ -> type_mismatch [rec_ty] [RecordTy []] )
  | List es ->
      map (fun e -> eval_expr e env) es >>| fun es -> {expr with data= List es}
  | Record fields ->
      map
        (fun (name, expr) -> eval_expr expr env >>= fun v -> Ok (name, v))
        fields
      >>= fun expr_fields ->
      map
        (fun (name, expr) ->
          match !(expr.ty) with
          | Some ty -> Ok (name, annotate ~loc:expr.loc ~ty:(Some ty) ty)
          | _ -> failwith "Type not found in record" )
        expr_fields
      >>| fun type_fields ->
      (* map (fun (name, expr) -> Ok (name, expr.ty)) fields >>| fun type_fields
         -> *)
      {expr with data= Record expr_fields; ty= ref (Some (RecordTy type_fields))}
  | _ -> invalid_expr ()

and eval_binop v1 v2 op =
  let ty1, ty2 =
    match (!(v1.ty), !(v2.ty)) with
    | Some ty1, Some ty2 -> (ty1, ty2)
    | Some ty1, _ -> (ty1, UnitTy)
    | _, Some ty2 -> (UnitTy, ty2)
    | _ -> failwith "Type not found in binary operation"
  in
  match op with
  | Add -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        Ok {v1 with data= IntLit (i1 + i2); ty= ref (Some IntTy)}
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | Sub -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        Ok {v1 with data= IntLit (i1 - i2); ty= ref (Some IntTy)}
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | Mult -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        Ok {v1 with data= IntLit (i1 * i2); ty= ref (Some IntTy)}
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | Div -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        Ok {v1 with data= IntLit (i1 / i2); ty= ref (Some IntTy)}
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | Eq -> (
    match (v1.data, v2.data) with
    (* Number *)
    | IntLit i1, IntLit i2 ->
        if i1 = i2 then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | IntLit _, _ | _, IntLit _ -> type_mismatch [IntTy] [ty1; ty2]
    (* String *)
    | StringLit s1, StringLit s2 ->
        if s1 = s2 then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | StringLit _, _ | _, StringLit _ -> type_mismatch [StringTy] [ty1; ty2]
    (* Boolean *)
    | True, True | False, False ->
        Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | True, False | False, True ->
        Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | True, _ | False, _ | _, True | _, False ->
        type_mismatch [BoolTy] [ty1; ty2]
    (* Other *)
    | _ -> is_not_type "Int, Boolean or String" v1 )
  | NotEq -> (
    match (v1.data, v2.data) with
    (* Number *)
    | IntLit i1, IntLit i2 ->
        if i1 <> i2 then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | IntLit _, _ | _, IntLit _ -> type_mismatch [IntTy] [ty1; ty2]
    (* String *)
    | StringLit s1, StringLit s2 ->
        if s1 <> s2 then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | StringLit _, _ | _, StringLit _ -> type_mismatch [StringTy] [ty1; ty2]
    (* Boolean *)
    | True, False | False, True ->
        Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | True, True | False, False ->
        Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | True, _ | False, _ | _, True | _, False ->
        type_mismatch [BoolTy] [ty1; ty2]
    (* Other *)
    | _ -> is_not_type "Int or String" v1 )
  | GreaterThan -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        if i1 > i2 then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | IntLit _, _ | _, IntLit _ -> type_mismatch [IntTy] [ty1; ty2]
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | GreaterOrEqual -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        if i1 >= i2 then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | IntLit _, _ | _, IntLit _ -> type_mismatch [IntTy] [ty1; ty2]
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | LessThan -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        if i1 < i2 then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | IntLit _, _ | _, IntLit _ -> type_mismatch [IntTy] [ty1; ty2]
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | LessOrEqual -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        if i1 <= i2 then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | IntLit _, _ | _, IntLit _ -> type_mismatch [IntTy] [ty1; ty2]
    | _ -> type_mismatch [IntTy] [ty1; ty2] )
  | And -> (
    match (v1.data, v2.data) with
    | True, True -> Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | True, False | False, True | False, False ->
        Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | _ -> type_mismatch [BoolTy] [ty1; ty2] )
  | Or -> (
    match (v1.data, v2.data) with
    | False, False -> Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | True, False | False, True | True, True ->
        Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | _ -> type_mismatch [BoolTy] [ty1; ty2] )

and eval_unop v op =
  let ty = Option.value ~default:UnitTy !(v.ty) in
  match op with
  | Minus -> (
    match v.data with
    | IntLit i -> Ok {v with data= IntLit (-i); ty= ref (Some IntTy)}
    | _ -> type_mismatch [IntTy] [ty] )
  | Negation -> (
    match v.data with
    | True -> Ok (annotate ~loc:v.loc ~ty:(Some BoolTy) False)
    | False -> Ok (annotate ~loc:v.loc ~ty:(Some BoolTy) True)
    | _ -> type_mismatch [BoolTy] [ty] )
(* | _ -> failwith "Invalid unary operator" *)

and find_id id env =
  match find_flat id.data env with
  | None -> id_not_found id
  | Some expr -> Ok expr

(* =============================================================================
   Expression Partial Evaluation functions
   ============================================================================= *)

(* See more: https://link.springer.com/chapter/10.1007/3-540-11980-9_13 *)
and partial_eval_expr expr expr_env =
  match expr.data with
  | Parenthesized e ->
      partial_eval_expr e expr_env
      >>= fun v -> Ok {expr with data= Parenthesized v}
  | BinaryOp (e1, e2, op) ->
      partial_eval_expr e1 expr_env
      >>= fun v1 ->
      partial_eval_expr e2 expr_env
      >>= fun v2 -> Ok {expr with data= BinaryOp (v1, v2, op)}
  | UnaryOp (e, op) ->
      partial_eval_expr e expr_env
      >>= fun v -> Ok {expr with data= UnaryOp (v, op)}
  | Identifier id -> find_id id expr_env
  (* TODO: Put more cases *)
  | _ -> Ok expr
