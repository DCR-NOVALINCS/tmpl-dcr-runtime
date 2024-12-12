open Syntax
open Errors
open Misc
open Env
open Monads.ResultMonad
(* open Printing *)

(* =============================================================================
   Expression Evaluation functions
   ============================================================================= *)

let rec eval_expr expr env =
  (* let open Misc.Printing in *)
  match expr.data with
  | Unit | True | False | IntLit _ | StringLit _ -> return expr
  | Ref expr_ref -> eval_expr !expr_ref env
  | Parenthesized e -> eval_expr e env
  | BinaryOp (l, r, op) ->
      (* Evaluate left expression *)
      eval_expr l env
      >>= fun lv ->
      (* Evaluate right expression *)
      eval_expr r env
      >>= fun rv ->
      (* Evaluate the expression according to [op] with corresponding values [lv] and [rv] *)
      eval_binop lv rv op
  | UnaryOp (e, op) ->
      (* Evaluate expression *)
      eval_expr e env
      >>= fun v ->
      (* Evaluate the expression according to [op] with corresponding value [v] *)
      eval_unop v op
  | Identifier id -> find_id id env
  | Trigger -> find_id {expr with data= trigger_id} env
  | PropDeref (e, p) -> (
      eval_expr e env
      >>= fun v ->
      match v.data with
      | Record fields -> (
          let fields =
            List.map (fun (name, expr) -> (name.data, expr)) fields
          in
          match List.assoc_opt p.data fields with
          | None -> property_not_found p v
          | Some v -> return (annotate ~loc:v.loc v.data) )
      | EventRef event_ref -> (
          let event = !event_ref in
          let {marking; io; _} = event.data in
          match p.data with
          | "value" ->
              ( match io.data with
              | Output expr ->
                  eval_expr expr env >>= fun value -> return (Ref (ref value))
              | _ -> return (Ref marking.data.value) )
              >>= fun value -> eval_expr {expr with data= value} env
          | _ -> property_not_found p v )
      | _ ->
          should_not_happen ~module_path:"evaluation.ml"
            ( "Tried to dereference a non-record value"
            ^ Unparser.PlainUnparser.unparse_expr v ) )
  | List elems ->
      map (fun elem -> eval_expr elem env) elems
      >>| fun elems -> {expr with data= List elems}
  | Range (s, e) ->
      eval_expr s env
      >>= (fun start_value ->
            eval_expr e env
            >>= fun end_value ->
            match (start_value.data, end_value.data) with
            | IntLit s, IntLit e ->
                let rec range start_int end_int =
                  if start_int > end_int then []
                  else start_int :: range (start_int + 1) end_int
                in
                range s e
                |> List.map (fun i -> annotate ~loc:expr.loc (IntLit i))
                |> return
            | _ -> type_mismatch ~loc:s.loc [IntTy; IntTy] [] )
      >>| fun es -> annotate ~loc:expr.loc ~ty:(Some (ListTy IntTy)) (List es)
  | Record fields ->
      eval_record_fields fields env
      >>| fun fields -> {expr with data= Record fields}
  | EventRef event_ref ->
      let event = !event_ref in
      let {marking; _} = event.data in
      let fields =
        [(annotate "value", annotate ~loc:event.loc !(marking.data.value).data)]
      in
      return {expr with data= Record fields}
(* | _ -> invalid_expr () *)

and eval_binop v1 v2 op =
  match op with
  | Add -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        return {v1 with data= IntLit (i1 + i2); ty= ref (Some IntTy)}
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid addition (+) between %s and %s"
             (Unparser.PlainUnparser.unparse_expr v1)
             (Unparser.PlainUnparser.unparse_expr v2) ) )
  | Sub -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        return {v1 with data= IntLit (i1 - i2); ty= ref (Some IntTy)}
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid subtraction (-) between %s and %s"
             (Unparser.PlainUnparser.unparse_expr v1)
             (Unparser.PlainUnparser.unparse_expr v2) ) )
  | Mult -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        return {v1 with data= IntLit (i1 * i2); ty= ref (Some IntTy)}
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid multiplication (*) between %s and %s"
             (Unparser.PlainUnparser.unparse_expr v1)
             (Unparser.PlainUnparser.unparse_expr v2) ) )
  | Div -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        return {v1 with data= IntLit (i1 / i2); ty= ref (Some IntTy)}
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid division (/) between %s and %s"
             (Unparser.PlainUnparser.unparse_expr v1)
             (Unparser.PlainUnparser.unparse_expr v2) ) )
  | Eq -> (
    match (v1.data, v2.data) with
    (* Number *)
    | IntLit i1, IntLit i2 ->
        if i1 = i2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    (* String *)
    | StringLit s1, StringLit s2 ->
        if s1 = s2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    (* Boolean *)
    | True, True | False, False ->
        return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | True, False | False, True ->
        return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    (* Other *)
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid equality (==) between %s and %s"
             (Unparser.PlainUnparser.unparse_expr v1)
             (Unparser.PlainUnparser.unparse_expr v2) ) )
  | NotEq -> (
    match (v1.data, v2.data) with
    (* Number *)
    | IntLit i1, IntLit i2 ->
        if i1 <> i2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    (* String *)
    | StringLit s1, StringLit s2 ->
        if s1 <> s2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    (* Boolean *)
    | True, False | False, True ->
        return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | True, True | False, False ->
        return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    (* Other *)
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid inequality (!=) between %s and %s"
             (Unparser.PlainUnparser.unparse_expr v1)
             (Unparser.PlainUnparser.unparse_expr v2) ) )
  | GreaterThan -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        if i1 > i2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid greater than (>) between %s and %s"
             (Unparser.PlainUnparser.unparse_expr v1)
             (Unparser.PlainUnparser.unparse_expr v2) ) )
  | GreaterOrEqual -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        if i1 >= i2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid greater or equal (>=) between %s and %s"
             (Unparser.PlainUnparser.unparse_expr v1)
             (Unparser.PlainUnparser.unparse_expr v2) ) )
  | LessThan -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        if i1 < i2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid less than (<) between %s and %s"
             (Unparser.PlainUnparser.unparse_expr v1)
             (Unparser.PlainUnparser.unparse_expr v2) ) )
  | LessOrEqual -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 ->
        if i1 <= i2 then return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
        else return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid less or equal (<=) between %s and %s"
             (Unparser.PlainUnparser.unparse_expr v1)
             (Unparser.PlainUnparser.unparse_expr v2) ) )
  | And -> (
    match (v1.data, v2.data) with
    | True, True -> return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | True, False | False, True | False, False ->
        return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid and (AND) between %s and %s"
             (Unparser.PlainUnparser.unparse_expr v1)
             (Unparser.PlainUnparser.unparse_expr v2) ) )
  | Or -> (
    match (v1.data, v2.data) with
    | False, False -> return (annotate ~loc:v1.loc ~ty:(Some BoolTy) False)
    | True, False | False, True | True, True ->
        return (annotate ~loc:v1.loc ~ty:(Some BoolTy) True)
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid or (OR) between %s and %s"
             (Unparser.PlainUnparser.unparse_expr v1)
             (Unparser.PlainUnparser.unparse_expr v2) ) )

and eval_unop v op =
  match op with
  | Minus -> (
    match v.data with
    | IntLit i -> return {v with data= IntLit (-i); ty= ref (Some IntTy)}
    | _ ->
        should_not_happen ~module_path:"evaluation.ml" "Invalid unary minus (-)"
    )
  | Negation -> (
    match v.data with
    | True -> return (annotate ~loc:v.loc ~ty:(Some BoolTy) False)
    | False -> return (annotate ~loc:v.loc ~ty:(Some BoolTy) True)
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          "Invalid unary negation (~)" )
(* | _ -> failwith "Invalid unary operator" *)

and find_id id env =
  match find_flat id.data env with
  | None -> id_not_found id
  | Some expr -> return expr

and eval_record_fields fields env =
  map
    (fun (name, expr) -> eval_expr expr env >>= fun v -> return (name, v))
    fields

and reference_of_event expr =
  match expr.data with
  | EventRef event_ref -> return event_ref
  | _ ->
      should_not_happen ~module_path:"evaluation.ml" "Invalid event reference"

and reference_of_expr expr =
  match expr.data with
  | Ref expr_ref -> return expr_ref
  | _ ->
      should_not_happen ~module_path:"evaluation.ml"
        "Invalid expression reference"

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
