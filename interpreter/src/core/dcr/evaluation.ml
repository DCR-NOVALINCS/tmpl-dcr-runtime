(* open Helper *)
open Ast
open Syntax
open Error
open Errors
open Unparser
open Common
open Env
open Monads.ResultMonad
open Printing

(* =============================================================================
   Expression Evaluation functions
   ============================================================================= *)

let rec eval_expr expr env =
  Logger.debug
    (Printf.sprintf "Evaluating fully the expression %s"
       (Colorized.unparse_expr expr) ) ;
  match expr.data with
  | Unit | BoolLit _ | IntLit _ | StringLit _ -> return expr
  | Ref expr_ref -> eval_expr !expr_ref env
  | Parenthesized e -> eval_expr e env
  | BinaryOp (l, r, op) ->
      Logger.debug
        (Printf.sprintf "Evaluating binary operator %s with %s"
           (Colorized.unparse_expr l) (Colorized.unparse_expr r) ) ;
      eval_binop l r op env
  | UnaryOp (e, op) -> eval_unop e op env
  | Identifier id ->
      let* v = find_id id env in
      eval_expr v env
  | Trigger ->
      let* v = find_id {expr with data= trigger_id} env in
      eval_expr v env
  | PropDeref (e, p) -> (
      let* v = eval_expr e env in
      match v.data with
      | Record fields ->
          let fields =
            List.map (fun (name, expr) -> (name.data, expr)) fields
          in
          eval_prop_deref fields p v
      | EventRef event_ref ->
          let event = !event_ref in
          let {marking; io; _} = event.data in
          let* fields =
            let* value =
              match io.data with
              | Output expr -> eval_expr expr env
              | Input _ ->
                  let {value; _} = marking.data in
                  return !value
            in
            return [("value", value)]
          in
          eval_prop_deref fields p v
      | _ ->
          should_not_happen ~module_path:"evaluation.ml"
            ( "Tried to dereference a non-record value "
            ^ Colorized.unparse_expr v ) )
  | List elems ->
      let+ elems = map (fun elem -> eval_expr elem env) elems in
      {expr with data= List elems}
  | Range (s, e) ->
      let* s = eval_expr s env in
      let* e = eval_expr e env in
      let* start, end_ =
        match (s.data, e.data) with
        | IntLit s, IntLit e -> return (s, e)
        | _ ->
            should_not_happen ~module_path:"evaluation.ml"
              (Printf.sprintf "Invalid range expression (%s..%s)"
                 (Colorized.unparse_expr s) (Colorized.unparse_expr e) )
      in
      let rec range f start_int end_int =
        if start_int > end_int then []
        else f start_int :: range f (start_int + 1) end_int
      in
      let list =
        range
          (fun i -> annotate ~loc:expr.loc ~ty:(Some IntTy) (IntLit i))
          start end_
      in
      return {expr with data= List list}
  | Record fields ->
      let+ fields = eval_record_fields fields env in
      {expr with data= Record fields}
  | EventRef event_ref ->
      let event = !event_ref in
      let {io; _} = event.data in
      let* event =
        match io.data with
        | Input _ -> return event
        | Output expr ->
            let* value = eval_expr expr env in
            return
              { event with
                data= {event.data with io= {io with data= Output value}} }
      in
      event_ref := event ;
      return {expr with data= EventRef event_ref}

(* | EventRef event_ref ->
    let event = !event_ref in
    let {io; _} = event.data in
    ( match io.data with
    | Output expr -> eval_expr expr env
    | Input _ ->
        return (annotate ~loc:expr.loc ~ty:(Some UnitTy) Unit)
        (* value_from_input_event event  *) )
    >>= fun value ->
    let fields = [(annotate "value", value)] in
    eval_expr {expr with data= Record fields} env *)
(* | _ -> invalid_expr () *)

and eval_binop v1 v2 op env =
  let rec eval_eq v1 v2 =
    match (v1, v2) with
    | IntLit i1, IntLit i2 -> return (i1 = i2)
    | StringLit s1, StringLit s2 -> return (s1 = s2)
    | BoolLit b1, BoolLit b2 -> return (b1 = b2)
    | _ -> should_not_happen "Invalid comparison"
  and eval_neq v1 v2 = eval_eq v1 v2
  and eval_gt v1 v2 =
    match (v1, v2) with
    | IntLit i1, IntLit i2 -> return (i1 > i2)
    | _ -> should_not_happen "Invalid comparison"
  and eval_lt v1 v2 =
    match (v1, v2) with
    | IntLit i1, IntLit i2 -> return (i1 < i2)
    | _ -> should_not_happen "Invalid comparison"
  and eval_lt_eq v1 v2 =
    let* is_lower = eval_lt v1 v2 in
    let* is_eq = eval_eq v1 v2 in
    return (is_lower || is_eq)
  and eval_gt_eq v1 v2 =
    let* is_greater = eval_gt v1 v2 in
    let* is_eq = eval_eq v1 v2 in
    return (is_greater || is_eq)
    (* and map_bool_value b = BoolLit b  *)
  in
  let* v1 = eval_expr v1 env in
  let* v2 = eval_expr v2 env in
  match (v1.data, v2.data, op) with
  | IntLit i1, IntLit i2, Add -> return {v1 with data= IntLit (i1 + i2)}
  | IntLit i1, IntLit i2, Sub -> return {v1 with data= IntLit (i1 - i2)}
  | IntLit i1, IntLit i2, Mult -> return {v1 with data= IntLit (i1 * i2)}
  | IntLit i1, IntLit i2, Div -> return {v1 with data= IntLit (i1 / i2)}
  | v1', v2', Eq ->
      let* value = eval_eq v1' v2' in
      return (annotate ~loc:v1.loc ~ty:(Some BoolTy) (BoolLit value))
  | v1', v2', NotEq ->
      let* value = eval_neq v1' v2' in
      return (annotate ~loc:v1.loc ~ty:(Some BoolTy) (BoolLit value))
  | v1', v2', GreaterThan ->
      let* value = eval_gt v1' v2' in
      return (annotate ~loc:v1.loc ~ty:(Some BoolTy) (BoolLit value))
  | v1', v2', GreaterOrEqual ->
      let* value = eval_gt_eq v1' v2' in
      return (annotate ~loc:v1.loc ~ty:(Some BoolTy) (BoolLit value))
  | v1', v2', LessThan ->
      let* value = eval_lt v1' v2' in
      return (annotate ~loc:v1.loc ~ty:(Some BoolTy) (BoolLit value))
  | v1', v2', LessOrEqual ->
      let* value = eval_lt_eq v1' v2' in
      return (annotate ~loc:v1.loc ~ty:(Some BoolTy) (BoolLit value))
  | BoolLit b1, BoolLit b2, And -> return {v1 with data= BoolLit (b1 && b2)}
  | BoolLit b1, BoolLit b2, Or -> return {v1 with data= BoolLit (b1 || b2)}
  | _ ->
      should_not_happen ~loc:v1.loc
        (Printf.sprintf "Cannot evaluate the binary operation between %s and %s"
           (Colorized.unparse_expr v1)
           (Colorized.unparse_expr v2) )

and eval_unop v op env =
  let* v = eval_expr v env in
  match (v.data, op) with
  | IntLit i, Minus -> return {v with data= IntLit (-i)}
  | BoolLit b, Negation -> return {v with data= BoolLit (not b)}
  | _ ->
      should_not_happen ~loc:v.loc
        (Printf.sprintf "Cannot evaluate the unary operation %s"
           (Colorized.unparse_expr v) )
(* | _ -> failwith "Invalid unary operator" *)

and find_id id env =
  match find_flat id.data env with
  | None -> id_not_found id
  | Some expr -> return expr

and eval_record_fields fields env =
  map
    (fun (name, expr) -> eval_expr expr env >>= fun v -> return (name, v))
    fields

and eval_prop_deref fields p v =
  match List.assoc_opt p.data fields with
  | None -> property_not_found p v
  | Some v -> return v

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

and partial_eval_expr expr expr_env =
  match expr.data with
  | Ref expr_ref -> partial_eval_expr !expr_ref expr_env
  | Parenthesized e -> partial_eval_expr e expr_env
  | BinaryOp (e1, e2, op) ->
      let* v1 = partial_eval_expr e1 expr_env in
      let* v2 = partial_eval_expr e2 expr_env in
      return {expr with data= BinaryOp (v1, v2, op)}
  | UnaryOp (e, op) ->
      let* v = partial_eval_expr e expr_env in
      return {expr with data= UnaryOp (v, op)}
  | Identifier id -> (
    match find_flat id.data expr_env with
    | None -> return expr
    | Some v -> partial_eval_expr v expr_env )
  | PropDeref (e, p) ->
      let* v = partial_eval_expr e expr_env in
      return {expr with data= PropDeref (v, p)}
  | List elems ->
      let+ elems = map (fun elem -> partial_eval_expr elem expr_env) elems in
      {expr with data= List elems}
  | Record fields ->
      let+ fields =
        map
          (fun (name, expr) ->
            partial_eval_expr expr expr_env >>= fun v -> return (name, v) )
          fields
      in
      {expr with data= Record fields}
  | _ -> return expr
