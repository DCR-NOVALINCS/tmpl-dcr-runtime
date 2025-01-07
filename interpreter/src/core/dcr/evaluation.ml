open Errors
open Ast
open Syntax
open Error
open Unparser
open Common
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
  | EventRef _event_ref -> return expr
  | Ref expr_ref -> eval_expr !expr_ref env
  | Parenthesized e -> eval_expr e env
  | BinaryOp (l, r, op) -> eval_binop l r op env
  | UnaryOp (e, op) -> eval_unop e op env
  | Identifier id -> find_id id env
  | Trigger -> eval_trigger expr env
  | PropDeref (e, p) -> eval_prop_deref e p expr env
  | List elems -> eval_list expr elems env
  | Range (s, e) -> eval_range expr s e env
  | Record fields -> eval_record_fields expr fields env

(* | _ -> invalid_expr expr *)

and eval_binop v1 v2 op env =
  let eval_truthful b env =
    if b then eval_expr (annotate ~loc:v1.loc ~ty:(Some BoolTy) True) env
    else eval_expr (annotate ~loc:v1.loc ~ty:(Some BoolTy) False) env
  in
  eval_expr v1 env
  >>= fun v1 ->
  eval_expr v2 env
  >>= fun v2 ->
  match op with
  | Add -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 -> eval_expr {v1 with data= IntLit (i1 + i2)} env
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid addition (%s + %s)"
             (Colorized.unparse_expr v1)
             (Colorized.unparse_expr v2) ) )
  | Sub -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 -> eval_expr {v1 with data= IntLit (i1 - i2)} env
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid subtraction (%s - %s)"
             (Colorized.unparse_expr v1)
             (Colorized.unparse_expr v2) ) )
  | Mult -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 -> eval_expr {v1 with data= IntLit (i1 * i2)} env
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid multiplication (%s * %s)"
             (Colorized.unparse_expr v1)
             (Colorized.unparse_expr v2) ) )
  | Div -> (
    match (v1.data, v2.data) with
    | IntLit i1, IntLit i2 -> eval_expr {v1 with data= IntLit (i1 / i2)} env
    | _ ->
        should_not_happen ~module_path:"evaluation.ml"
          (Printf.sprintf "Invalid division (%s / %s)"
             (Colorized.unparse_expr v1)
             (Colorized.unparse_expr v2) ) )
  | Eq ->
      ( match (v1.data, v2.data) with
      | IntLit i1, IntLit i2 -> return (i1 = i2)
      | StringLit s1, StringLit s2 -> return (s1 = s2)
      | True, True | False, False -> return true
      | _ ->
          should_not_happen ~module_path:"evaluation.ml"
            "Invalid equality operator" )
      >>= fun b -> eval_truthful b env
  | NotEq ->
      ( match (v1.data, v2.data) with
      | IntLit i1, IntLit i2 -> return (i1 <> i2)
      | StringLit s1, StringLit s2 -> return (s1 <> s2)
      | True, True | False, False -> return false
      | True, False | False, True -> return true
      | _ ->
          should_not_happen ~module_path:"evaluation.ml"
            "Invalid inequality operator" )
      >>= fun b -> eval_truthful b env
  | GreaterThan ->
      ( match (v1.data, v2.data) with
      | IntLit i1, IntLit i2 -> return (i1 > i2)
      | _ ->
          should_not_happen ~module_path:"evaluation.ml"
            "Invalid greater than operator" )
      >>= fun b -> eval_truthful b env
  | GreaterOrEqual ->
      ( match (v1.data, v2.data) with
      | IntLit i1, IntLit i2 -> return (i1 >= i2)
      | _ ->
          should_not_happen ~module_path:"evaluation.ml"
            "Invalid greater or equal operator" )
      >>= fun b -> eval_truthful b env
  | LessThan ->
      ( match (v1.data, v2.data) with
      | IntLit i1, IntLit i2 -> return (i1 < i2)
      | _ ->
          should_not_happen ~module_path:"evaluation.ml"
            (Printf.sprintf "Invalid less than operator with %s and %s"
               (Colorized.unparse_expr v1)
               (Colorized.unparse_expr v2) ) )
      >>= fun b -> eval_truthful b env
  | LessOrEqual ->
      ( match (v1.data, v2.data) with
      | IntLit i1, IntLit i2 -> return (i1 <= i2)
      | _ ->
          should_not_happen ~module_path:"evaluation.ml"
            "Invalid less or equal operator" )
      >>= fun b -> eval_truthful b env
  | And -> (
    match (v1.data, v2.data) with
    | True, True -> eval_expr (annotate ~loc:v1.loc ~ty:(Some BoolTy) True) env
    | False, _ | _, False ->
        eval_expr (annotate ~loc:v1.loc ~ty:(Some BoolTy) False) env
    | _ -> should_not_happen ~module_path:"evaluation.ml" "Invalid and operator"
    )
  | Or -> (
    match (v1.data, v2.data) with
    | True, _ | _, True ->
        eval_expr (annotate ~loc:v1.loc ~ty:(Some BoolTy) True) env
    | False, False ->
        eval_expr (annotate ~loc:v1.loc ~ty:(Some BoolTy) False) env
    | _ -> should_not_happen ~module_path:"evaluation.ml" "Invalid or operator"
    )
(* | _ -> failwith "Invalid binary operator" *)

and eval_unop v op env =
  eval_expr v env
  >>= fun v ->
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

and eval_trigger expr env =
  find_id {expr with data= trigger_id} env
  >>= fun trigger -> eval_expr trigger env

and eval_prop_deref e p expr env =
  eval_expr e env
  >>= fun v ->
  match v.data with
  | Record fields -> (
      let fields = List.map (fun (name, expr) -> (name.data, expr)) fields in
      match List.assoc_opt p.data fields with
      | None -> property_not_found p v
      | Some v -> eval_expr (annotate ~loc:v.loc v.data) env )
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
        ("Tried to dereference a non-record value " ^ Plain.unparse_expr v)

and eval_list expr elems env =
  map (fun elem -> eval_expr elem env) elems
  >>| fun elems -> {expr with data= List elems}

and eval_range expr start_value end_value env =
  eval_expr start_value env
  >>= fun start_value ->
  eval_expr end_value env
  >>= fun end_value ->
  ( match (start_value.data, end_value.data) with
  | IntLit s, IntLit e ->
      let rec range start_int end_int =
        if start_int > end_int then []
        else start_int :: range (start_int + 1) end_int
      in
      return (List.map (fun i -> {expr with data= IntLit i}) (range s e))
  | _ ->
      should_not_happen ~module_path:"evaluation.ml"
        (Printf.sprintf "Invalid range expression (%s..%s)"
           (Colorized.unparse_expr start_value)
           (Colorized.unparse_expr end_value) ) )
  >>| fun es -> annotate ~loc:expr.loc ~ty:(Some (ListTy IntTy)) (List es)

and find_id id env =
  match find_flat id.data env with
  | None -> id_not_found id
  | Some expr -> return expr

and eval_record_fields expr fields env =
  map
    (fun (name, expr) -> eval_expr expr env >>= fun v -> return (name, v))
    fields
  >>| fun fields -> {expr with data= Record fields}

and _reference_of_event expr =
  match expr.data with
  | EventRef event_ref -> return event_ref
  | _ ->
      should_not_happen ~module_path:"evaluation.ml" "Invalid event reference"

and _reference_of_expr expr =
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
  let partial_eval_record_fields fields expr_env =
    map
      (fun (name, expr) ->
        partial_eval_expr expr expr_env >>= fun v -> return (name, v) )
      fields
  in
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
  | Identifier id ->
      find_id id expr_env >>= fun v -> partial_eval_expr v expr_env
  (* | Trigger -> find_id {expr with data= trigger_id} expr_env *)
  | PropDeref (e, p) -> (
      partial_eval_expr e expr_env
      >>= fun v ->
      match v.data with
      | Record fields -> (
          let fields =
            List.map (fun (name, expr) -> (name.data, expr)) fields
          in
          match List.assoc_opt p.data fields with
          | None -> property_not_found p v
          | Some v -> return v )
      | _ -> return {expr with data= PropDeref (v, p)} )
  | List elems ->
      map (fun elem -> partial_eval_expr elem expr_env) elems
      >>| fun elems -> {expr with data= List elems}
  | Range (s, e) ->
      partial_eval_expr s expr_env
      >>= fun start_value ->
      partial_eval_expr e expr_env
      >>= fun end_value ->
      return {expr with data= Range (start_value, end_value)}
  | Record fields ->
      partial_eval_record_fields fields expr_env
      >>| fun fields -> {expr with data= Record fields}
  (* | EventRef event_ref ->
      let event = !event_ref in
      let {io; _} = event.data in
      ( match io.data with
      | Output expr -> partial_eval_expr expr expr_env
      | Input _ -> value_from_input_event event )
      >>= fun value ->
      let fields = [(annotate "value", value)] in
      return {expr with data= Record fields} *)
  (* TODO: Put more cases *)
  | _ -> return expr
