open Misc.Env 
open Misc.Monads.ResultMonad
open Syntax
open Errors

(*
===============================================================
  Expression Evaluation functions
===============================================================
*)

let rec eval_expr expr env =
  match expr.data with
  | Unit | True | False -> Ok ({expr with ty = (ref (Some BoolTy))})
  | IntLit _ as data -> Ok ({expr with data; ty = (ref (Some IntTy))})
  | StringLit _ as data-> Ok ({expr with data; ty = (ref (Some StringTy))})
  | Parenthesized e -> eval_expr e env
  | BinaryOp (e1, e2, op) -> 
    eval_expr e1 env
    >>= fun v1 -> 
    eval_expr e2 env
    >>= fun v2 ->
    eval_binop v1 v2 op
  | UnaryOp (e, op) -> 
    eval_expr e env
    >>= fun v -> 
    eval_unop v op
  | Identifier id -> find_id id env
  | Trigger -> find_id (annotate ~loc:expr.loc ~ty:!(expr.ty) "@trigger") env
  | PropDeref (e, p) -> 
    eval_expr e env
    >>= fun v -> 
    ( match v.data with
    | Record fields -> 
      let fields = List.map (fun (prop, expr) -> (prop.data, expr.data) ) fields in
      ( match List.assoc_opt p.data fields with
      | None -> property_not_found p v
      | Some v -> Ok (annotate ~loc:p.loc ~ty:!(p.ty) v) )
    | _ -> is_not_type "Record" v )
  | List es -> 
    map 
      (fun e -> eval_expr e env)
      es
    >>| fun es -> { expr with data = List es }
  | Record fields ->
    map
      (fun (name, expr) -> 
        eval_expr expr env
        >>= fun v -> Ok (name, v))
      fields 
    >>| fun fields -> { expr with data = Record fields } 
  | _ -> invalid_expr ()

and eval_binop v1 v2 op = 
  match op with
  | Add -> 
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> Ok ({v1 with data = IntLit (i1 + i2); ty = (ref (Some IntTy))})
    | IntLit _, _ -> is_not_type "Int" v2
    | _ -> is_not_type "Int" v1
    )
  
  | Sub -> 
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> Ok ({v1 with data = IntLit (i1 - i2); ty = (ref (Some IntTy))})
    | IntLit _, _ -> is_not_type "Int" v2
    | _ -> is_not_type "Int" v1
    )

  | Mult -> 
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> Ok ({v1 with data = IntLit (i1 * i2); ty = (ref (Some IntTy))})
    | IntLit _, _ -> is_not_type "Int" v2
    | _ -> is_not_type "Int" v1
    )

  | Div -> 
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> Ok ({v1 with data = IntLit (i1 / i2); ty = (ref (Some IntTy))})
    | IntLit _, _ -> is_not_type "Int" v2
    | _ -> is_not_type "Int" v1
    )

  | Eq ->
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> 
      if i1 = i2 
        then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True) 
      else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | StringLit s1, StringLit s2 -> 
      if s1 = s2 
        then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True) 
      else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | True, True -> Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | False, False -> Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | _ -> is_not_type "Int or String" v1
    )

  | NotEq ->
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> 
      if i1 <> i2 
        then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True) 
      else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | StringLit s1, StringLit s2 ->
      if s1 <> s2
        then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True) 
      else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | True, False -> Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | False, True -> Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | _ -> is_not_type "Int or String" v1
    )

  | GreaterThan ->
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> 
      if i1 > i2 
        then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True) 
      else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | _ -> is_not_type "Int" v1
    )

  | GreaterOrEqual ->
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> 
      if i1 >= i2 
        then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True) 
      else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | _ -> is_not_type "Int" v1
    )

  | LessThan ->
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> 
      if i1 < i2 
        then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True) 
      else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | _ -> is_not_type "Int" v1
    )

  | LessOrEqual ->
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> 
      if i1 <= i2 
        then Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True) 
      else Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | _ -> is_not_type "Int" v1
    )

  | And ->
    ( match v1.data, v2.data with
    | True, True -> Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | _ -> Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    )

  | Or ->
    ( match v1.data, v2.data with
    | False, False -> Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | _ -> Ok (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    )

and eval_unop v op = 
  match op with
  | Minus -> 
    ( match v.data with
    | IntLit i -> Ok ({v with data = IntLit (-i); ty = (ref (Some IntTy))})
    | _ -> is_not_type "Int" v
    )
  | Negation -> 
    ( match v.data with
    | True -> Ok (annotate ~loc:v.loc ~ty:(Some BoolTy) False)
    | False -> Ok (annotate ~loc:v.loc ~ty:(Some BoolTy) True)
    | _ -> is_not_type "Bool" v
    )
  (* | _ -> failwith "Invalid unary operator" *)

and find_id id env = 
  match find_flat id.data env with
  | None -> id_not_found id
  | Some expr -> Ok expr

(*
===============================================================
  Expression Partial Evaluation functions
===============================================================
*)

(* See more: https://link.springer.com/chapter/10.1007/3-540-11980-9_13 *)
and partial_eval_expr expr expr_env = 
  match expr.data with 
  | Parenthesized e -> 
    partial_eval_expr e expr_env
    >>= fun v -> Ok ({expr with data = Parenthesized v})
  | BinaryOp (e1, e2, op) -> 
    partial_eval_expr e1 expr_env
    >>= fun v1 -> 
    partial_eval_expr e2 expr_env
    >>= fun v2 ->
    Ok ({expr with data = BinaryOp (v1, v2, op)})
  | UnaryOp (e, op) -> 
    partial_eval_expr e expr_env
    >>= fun v -> Ok ({expr with data = UnaryOp (v, op)})
  | Identifier id -> find_id id expr_env
  (* TODO: Put more cases *)
  | _ -> Ok expr
