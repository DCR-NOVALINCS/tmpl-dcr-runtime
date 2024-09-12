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

(* and find_event ~id env = 
  find_flat id env *)

(* and remove_event ~id program = 
  let events = List.filter (fun e -> let (id', _) = e.info in id <> id') program.events in
  { program with events }

and add_event event program = 
  let events = event :: program.events in
  { program with events } *)

and update_event event program = 
  let events = List.map (fun e -> 
    let (id, _) = e.info in
    let (id', _) = event.info in
    if id = id' then event else e) program.events in
  { program with events }

and is_enabled event program (event_env, expr_env) = 
  let relations = program.relations in
  let enabled = event.marking.included in
  List.fold_left 
    (fun enabled relation -> enabled && is_enabled_by relation event (event_env, expr_env))
    enabled relations

and is_enabled_by relation event (_event_env, _expr_env) =
  let (_id, _) = event.info in
  match relation with
  | ControlRelation (_from, _guard, _dest, _op) ->
    (* TODO: *)
      (* Evaluate the guard expression *)
      (* check_guard guard expr_env
      >>= fun _guard_value ->
      (find_flat _from event_env) |> Option.to_result
      >>= fun from_event ->
      (find_flat _dest event_env) |> Option.to_result
      |> Result.is_ok *)
      true
      
      
  | _ -> true

and check_guard guard env = 
  eval_expr guard env
  >>= fun guard_value ->
  match guard_value with
  | True -> Ok true
  | False -> Ok false
  | _ as expr -> Error ("Invalid guard value " ^ string_of_expr expr)

and propagate_effects event (event_env, expr_env) program = 
  let relations = program.relations in
  fold_left_result
    (fun program relation -> 
      propagate_effect relation event (event_env, expr_env) program
      |> function
      | Ok program' -> Ok program'
      | Error _ -> Ok program)  
    program relations

and propagate_effect relation _event (event_env, expr_env) program = 
  match relation with 
  | SpawnRelation (_from, guard, _dest) -> 
    check_guard guard expr_env
    >>= fun _ ->
    Ok program 
  | ControlRelation (_from, guard, _dest, _op) -> 
    check_guard guard expr_env
    >>= fun _ ->
    (* Get the event [dest] *)
    (find_flat _dest event_env |> Option.to_result ~none:(event_not_found _dest |> Result.get_error) )
    >>= fun dest_event ->
    (* According to [op], apply the effect on the marking for both events *)
    (* Order of applying the relations: response -> exclude -> include -> other (do nothing)  *)
    match _op with
    | Response -> 
      let { marking = dest_marking; _ } = dest_event in
      let dest_marking = { dest_marking with pending = true } in
      let dest_event = { dest_event with marking = dest_marking } in
      Ok (update_event dest_event program)
    | Exclude -> 
      let { marking = dest_marking; _ } = dest_event in
      let dest_marking = { dest_marking with included = false } in
      let dest_event = { dest_event with marking = dest_marking } in
      Ok (update_event dest_event program)
    | Include -> 
      let { marking = dest_marking; _ } = dest_event in
      let dest_marking = { dest_marking with included = true } in
      let dest_event = { dest_event with marking = dest_marking } in
      Ok (update_event dest_event program)
    | _ -> Ok program
    

and record_event event = 
  let { marking; _ } = event in
  Record [
    ("value", marking.value)
  ]

(*
===============================================================
  Error messages
===============================================================
*)

and event_not_found event = Error Printf.(sprintf "Event %s not found" event)

and id_not_found id = Error Printf.(sprintf "Identifier %s not found" id)

and event_not_enabled event = 
  let (id, _) = event.info in
  Error Printf.(sprintf "Event %s is not enabled" id)

(*
===============================================================
  Available functions
===============================================================
*)

let rec execute ~event_id ?(expr = Unit) program  = 
  (* 
    TODO: 
    - Find the event in the program [X]
    - Check if the event is enabled [+/-]
    - Execute the event
      - Update the event marking [X]
      - Execute the effects of the relations (propagate the relation effects) []
  *)
  preprocess_program program
  >>= fun (event_env, expr_env) ->

  match find_flat event_id event_env with
  | None -> event_not_found event_id
  | Some event -> 
    ( is_enabled event program (event_env, expr_env) |> function
    | false -> event_not_enabled event
    | true -> Ok program 
    >>= begin match event.io with
      | Input _ -> execute_event event expr expr_env
      | Output data_expr -> execute_event event data_expr expr_env
      end
    >>= propagate_effects event (event_env, expr_env)
    )
    
and preprocess_program program = 
  fold_left_result
    (fun env event -> 
      let (id , _) = event.info in
      Ok (bind id event env))
    empty_env program.events
  >>= fun event_env ->

  fold_left_result
    (fun env event -> 
      let (id, _) = event.info in
      Ok (bind id (record_event event) env))
    empty_env program.events
  >>= fun expr_env ->

  Ok (event_env, expr_env)
      
and execute_event event expr env program = 
  eval_expr expr env
  >>= fun expr ->
  let marking = { event.marking with executed = true; value = expr } in
  let event = { event with marking } in
  Ok (update_event event program)

and view ?(filter = (fun _ _ -> true)) program = 
  preprocess_program program 
  >>= fun (event_env, expr_env) ->
  List.filter (filter (event_env, expr_env)) program.events
  |> List.map (fun event -> string_of_event event)
  |> String.concat "\n" 
  |> Result.ok

and view_enabled program =
  view ~filter:(fun (event_env, expr_env) event -> 
    is_enabled event program (event_env, expr_env)) program


