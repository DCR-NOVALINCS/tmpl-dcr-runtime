open Syntax
open Misc.Monads

(*
===============================================================
  Expression Evaluation functions
===============================================================
*)

open Misc.Env 


let rec eval_expr expr env =
  match expr with
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
  | Trigger -> find_id "trigger" env
  | PropDeref (e, p) -> 
    eval_expr e env
    >>= fun v -> 
    ( match v with
    | Record fields -> 
      ( match List.assoc_opt p fields with
      | None -> id_not_found p
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
  | _ -> Ok (IntLit 0)

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
  | None -> id_not_found id
  | Some expr -> Ok expr

(*
===============================================================
  Auxiliary functions
===============================================================
*)

and find_event ~id program = 
  List.find_opt (fun e -> let (id', _) = e.info in id = id') program.events

and remove_event ~id program = 
  let events = List.filter (fun e -> let (id', _) = e.info in id <> id') program.events in
  { program with events }

and add_event event program = 
  let events = event :: program.events in
  { program with events }

and is_enabled event program env = 
  let relations = program.relations in
  List.fold_left 
    (fun enabled relation -> enabled && is_enabled_by relation event env)
    true relations

and is_enabled_by relation event env =
  let _id = fst event.info in
  match relation with
  | SpawnRelation _ -> true
  | ControlRelation (_from, guard, _dest, op) ->
      let guard_value = eval_expr guard env in
      match guard_value with
      | Ok True -> 
        (match op with 
        | Condition -> false
        | Milestone -> false 
        | _ -> true )
      | _ -> false 


(*
===============================================================
  Error messages
===============================================================
*)

and event_not_found event = Error Printf.(sprintf "Event %s not found" event)

and id_not_found id = Error Printf.(sprintf "Identifier %s not found" id)


(*
===============================================================
  Available functions
===============================================================
*)

let execute ~event_id ~_expr env  program  = 
  (* 
    TODO: 
    - Find the event in the program [X]
    - Check if the event is enabled []
    - Execute the event
      - Update the event marking []
      - Execute the effects of the relations []
    - Update the program []
  *)
  match find_event ~id:event_id program with
  | None -> event_not_found event_id
  | Some event -> 
    if not (is_enabled event program env) then Ok program
    else 
      let env = empty_env in
      eval_expr _expr env 
      >>= fun value ->
      print_endline @@ "Value: " ^ (string_of_expr value); 
      Ok program
      

and view program = string_of_program program |> print_endline




