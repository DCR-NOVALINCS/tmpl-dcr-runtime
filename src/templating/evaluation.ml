open Misc.Env 
open Misc.Monads 
open Syntax

(*
===============================================================
  Expression Evaluation functions
===============================================================
*)

let rec eval_expr expr env =
  match expr with
  | Unit -> Ok Unit
  | True -> Ok True
  | False -> Ok False
  | IntLit i -> Ok (IntLit i)
  | StringLit s -> Ok (StringLit s)
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
  | Trigger -> find_id "@trigger" env
  | PropDeref (e, p) -> 
    eval_expr e env
    >>= fun v -> 
    ( match v with
    | Record fields -> 
      ( match List.assoc_opt p fields with
      | None -> Error ("Property " ^ p ^ " not found")
      | Some v -> Ok v )
    | _ -> failwith "Invalid property dereference" )
  | List _es -> Ok (List []) (* TODO: *)
  | Record fields -> 
    fold_left_result 
      (fun fields (name, e) -> 
        eval_expr e env
        >>| fun v -> (name, v) :: fields)
      [] fields
    >>| fun fields -> Record fields
  (* | _ -> Ok (IntLit 0) *)

and eval_binop v1 v2 op = 
  match op with
  | Add -> 
    (match v1, v2 with
    | IntLit i1, IntLit i2 -> Ok (IntLit (i1 + i2))
    | _ -> failwith "Invalid arguments for Add")

  | Mult -> 
    (match v1, v2 with
    | IntLit i1, IntLit i2 -> Ok (IntLit (i1 * i2))
    | _ -> failwith "Invalid arguments for Mult")

  | Eq ->
    (match v1, v2 with
    | IntLit i1, IntLit i2 -> 
      if i1 = i2 then Ok True else Ok False
    | StringLit s1, StringLit s2 -> 
      if s1 = s2 then Ok True else Ok False
    | True, True -> Ok True
    | False, False -> Ok True
    | _ -> failwith "Invalid arguments for Eq")

  | NotEq ->
    (match v1, v2 with
    | IntLit i1, IntLit i2 -> 
      if i1 <> i2 then Ok True else Ok False
    | StringLit s1, StringLit s2 -> 
      if s1 <> s2 then Ok True else Ok False
    | True, False -> Ok True
    | False, True -> Ok True
    | _ -> failwith "Invalid arguments for NotEq")

  | GreaterThan ->
    (match v1, v2 with
    | IntLit i1, IntLit i2 -> 
      if i1 > i2 then Ok True else Ok False
    | _ -> failwith "Invalid arguments for GreaterThan")

  | LessThan ->
    (match v1, v2 with
    | IntLit i1, IntLit i2 -> 
      if i1 < i2 then Ok True else Ok False
    | _ -> failwith "Invalid arguments for LessThan")

  | And ->
    (match v1, v2 with
    | True, True -> Ok True
    | _ -> Ok False)

  | Or ->
    (match v1, v2 with
    | False, False -> Ok False
    | _ -> Ok True)
  (* | _ -> failwith "Invalid binary operator" *)

and eval_unop v op = 
  match op with
  | Minus -> 
    (match v with
    | IntLit i -> Ok (IntLit (-i))
    | _ -> failwith "Invalid argument for Minus")
  | Negation -> 
    (match v with
    | True -> Ok False
    | False -> Ok True
    | _ -> failwith "Invalid argument for Negation")
  (* | _ -> failwith "Invalid unary operator" *)

and find_id id env = 
  match find_flat id env with
  | None -> Error ("Identifier " ^ id ^ " not found")
  | Some expr -> Ok expr
