open Misc.Env 
open Misc.Monads 
open Syntax

(*
===============================================================
  Error message functions
===============================================================
*)

let property_not_found p e = 
  Error {
    location = e.loc
    ; message = "Property " ^ p.data ^ " not found in " ^ string_of_expr e
    ; filepath = ""
  }

and is_not_type expected expr =  
  Error {
    location = expr.loc
    ; message = Printf.sprintf "Expected type %s, but got %s" expected (string_of_expr expr)
    ; filepath = ""
  }

and invalid_expr ?(loc = Nowhere) () = 
  Error {
    location = loc
    ; message = "Invalid expression"
    ; filepath = ""
  }

and id_not_found id = 
  Error {
    location = id.loc
    ; message = "Identifier " ^ id.data ^ " not found"
    ; filepath = ""
  }

(*
===============================================================
  Expression Evaluation functions
===============================================================
*)

let rec eval_expr expr env =
  match expr.data with
  | Unit | True | False -> Ok expr
  | IntLit _ as data -> Ok ({expr with data})
  | StringLit _ as data-> Ok ({expr with data})
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
      ( match List.assoc_opt p fields with
      | None -> property_not_found p v
      | Some v -> Ok v )
    | _ -> is_not_type "Record" v )
  | List es -> 
    fold_left_result 
      (fun result e -> 
        eval_expr e env
        >>= fun v -> Ok (v :: result)
        )
      [] es
    >>| fun es -> { expr with data = List es }
  | Record fields -> 
    fold_left_result 
      (fun fields (name, e) -> 
        eval_expr e env
        >>| fun v -> (name, v) :: fields)
      [] fields
    >>| fun fields -> { expr with data = Record fields }
  (* | _ -> Ok (IntLit 0) *)
  | _ -> invalid_expr ()

and eval_binop v1 v2 op = 
  match op with
  | Add -> 
    (match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> Ok ({v1 with data = IntLit (i1 + i2)})
    | IntLit _, _ -> is_not_type "Int" v2
    | _ -> is_not_type "Int" v1
    )

  | Mult -> 
    (match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> Ok ({v1 with data = IntLit (i1 * i2)})
    | IntLit _, _ -> is_not_type "Int" v2
    | _ -> is_not_type "Int" v1
    )

  | Eq ->
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> 
      if i1 = i2 then Ok (annotate ~loc:v1.loc True) else Ok (annotate ~loc:v1.loc False)
    | StringLit s1, StringLit s2 -> 
      if s1 = s2 then Ok (annotate ~loc:v1.loc True) else Ok (annotate ~loc:v1.loc False)
    | True, True -> Ok (annotate ~loc:v1.loc True)
    | False, False -> Ok (annotate ~loc:v1.loc True)
    | _ -> is_not_type "Int or String" v1
    )

  | NotEq ->
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> 
      if i1 <> i2 then Ok (annotate ~loc:v1.loc True) else Ok (annotate ~loc:v1.loc False)
    | StringLit s1, StringLit s2 ->
      if s1 <> s2 then Ok (annotate ~loc:v1.loc True) else Ok (annotate ~loc:v1.loc False)
    | True, False -> Ok (annotate ~loc:v1.loc True)
    | False, True -> Ok (annotate ~loc:v1.loc True)
    | _ -> is_not_type "Int or String" v1
    )

  | GreaterThan ->
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> 
      if i1 > i2 then Ok (annotate ~loc:v1.loc True) else Ok (annotate ~loc:v1.loc False)
    | _ -> is_not_type "Int" v1
    )

  | GreaterOrEqual ->
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> 
      if i1 >= i2 then Ok (annotate ~loc:v1.loc True) else Ok (annotate ~loc:v1.loc False)
    | _ -> is_not_type "Int" v1
    )

  | LessThan ->
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> 
      if i1 < i2 then Ok (annotate ~loc:v1.loc True) else Ok (annotate ~loc:v1.loc False)
    | _ -> is_not_type "Int" v1
    )

  | LessOrEqual ->
    ( match v1.data, v2.data with
    | IntLit i1, IntLit i2 -> 
      if i1 <= i2 then Ok (annotate ~loc:v1.loc True) else Ok (annotate ~loc:v1.loc False)
    | _ -> is_not_type "Int" v1
    )

  | And ->
    ( match v1.data, v2.data with
    | True, True -> Ok (annotate ~loc:v1.loc True)
    | _ -> Ok (annotate ~loc:v1.loc False)
    )

  | Or ->
    ( match v1.data, v2.data with
    | False, False -> Ok (annotate ~loc:v1.loc False)
    | _ -> Ok (annotate ~loc:v1.loc True)
    )

and eval_unop v op = 
  match op with
  | Minus -> 
    ( match v.data with
    | IntLit i -> Ok ({v with data = IntLit (-i)})
    | _ -> is_not_type "Int" v
    )
  | Negation -> 
    ( match v.data with
    | True -> Ok (annotate ~loc:v.loc False)
    | False -> Ok (annotate ~loc:v.loc True)
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
and partial_eval_expr expr = 
  match expr.data with 
  | Parenthesized e -> 
    partial_eval_expr e
    >>= fun v -> Ok ({expr with data = Parenthesized v})
  | BinaryOp (e1, e2, op) -> 
    partial_eval_expr e1
    >>= fun v1 -> 
    partial_eval_expr e2
    >>= fun v2 ->
    Ok ({expr with data = BinaryOp (v1, v2, op)})
  | UnaryOp (e, op) -> 
    partial_eval_expr e
    >>= fun v -> Ok ({expr with data = UnaryOp (v, op)})
  (* TODO: Put more cases *)
  | _ -> Ok expr
