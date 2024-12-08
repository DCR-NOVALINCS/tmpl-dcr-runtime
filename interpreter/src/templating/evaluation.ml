open Misc
open Env
open Monads.ResultMonad
open Printing
open Syntax
open Errors

(* =============================================================================
   Expression Evaluation functions
   ============================================================================= *)

let rec eval_expr expr env =
  (* let open Misc.Printing in *)
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
      >>= fun v1 ->
      eval_expr e2 env
      >>= fun v2 ->
      (* Logger.debug @@ "Expr env" ;
         Logger.debug @@ string_of_env Unparser.PlainUnparser.unparse_expr env ; *)
      eval_binop v1 v2 op
  | UnaryOp (e, op) -> eval_expr e env >>= fun v -> eval_unop v op
  | Identifier id -> find_id id env
  | Trigger -> find_id {expr with data= trigger_id} env
  | PropDeref (e, p) -> (
      (* Debug expr env *)
      Logger.debug @@ "Expr env in prop deref" ;
      Logger.debug @@ string_of_env Unparser.PlainUnparser.unparse_expr env ;
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
          | Some v -> return (annotate ~loc:v.loc ~ty:(Some rec_ty) v.data) )
      | _ ->
          should_not_happen ~module_path:"evaluation.ml"
            ( "Tried to dereference a non-record value "
            ^ Unparser.PlainUnparser.unparse_expr v ) )
  | List es ->
      map (fun e -> eval_expr e env) es >>| fun es -> {expr with data= List es}
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
                |> List.map (fun i ->
                       annotate ~loc:expr.loc ~ty:(Some IntTy) (IntLit i) )
                |> return
            | _ -> type_mismatch ~loc:s.loc [IntTy] [] )
      >>| fun es -> annotate ~loc:expr.loc ~ty:(Some (ListTy IntTy)) (List es)
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
  (* ( match (!(v1.ty), !(v2.ty)) with
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
     >>= fun (ty1, ty2) -> *)
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
  | None ->
      (* Logger.debug
         @@ Printf.sprintf "Expr env: %s"
              (string_of_env Unparser.PlainUnparser.unparse_expr env) ; *)
      id_not_found id
  | Some expr -> return expr

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
