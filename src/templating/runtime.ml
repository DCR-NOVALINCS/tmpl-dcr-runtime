open Syntax
open Misc.Monads
open Misc.Env
open Evaluation

(*
===============================================================
  Auxiliary functions
===============================================================
*)

let rec update_event event program = 
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
  | ControlRelation (_from, _guard, _dest, _op, _annot) ->
    (* TODO: *)
    begin match find_flat _dest _event_env with
    | Some dest_event when _id = fst dest_event.info -> 
      begin match find_flat _from _event_env with
      | Some from_event -> 
        let { marking = from_marking; _ } = from_event in
        let { marking = dest_marking; _ } = dest_event in
        begin match _op with 
        | Condition -> from_marking.included && from_marking.executed
                      && dest_marking.included
        | Milestone -> (not from_marking.pending) && from_marking.included
                      && dest_marking.included
        | _ -> true 
      end
      | None -> true
      end
    | _ -> true
    
    end
  | _ -> true

and check_guard guard env = 
  eval_expr guard env
  >>= fun guard_value ->
  match guard_value with
  | True -> Ok true
  | False -> Ok false
  | _ as expr -> invalid_guard_value expr

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
  | SpawnRelation (_from, _guard, _spawn_prog, _annot) -> 
    (* check_guard guard expr_env
    >>= fun _ -> *)
    (* TODO: Check if the guard evaluates to True *)
    (*
      TODO: 
      Get events / isnts / relations [X]
      Alpha - renaming local events [+/-]
      Bind "@trigger" in the env [X]
      Instantiate all insts [X]
      Put in the program [X] 
    *)
    let (_spawn_events, _spawn_insts, _spawn_relations) = _spawn_prog in

    (* Rename the event ids to new ones, to prevent id clashing *)
    fresh_event_ids _spawn_events _spawn_relations []
    >>= fun (_spawn_events, _spawn_relations) ->
      
    (* Begin new env scope and bind "@trigger" *)
    Ok (begin_scope expr_env)
    >>= fun expr_env ->
    Ok (bind "@trigger" (record_event _event) expr_env)
    >>= fun _expr_env ->
    print_endline "Expr env:";
    print_endline (string_of_env string_of_expr _expr_env);

    (* Update values of the event inside of the spawn *)
    fold_left_result
      (fun events event -> 
        let { marking; io; _ } = event in 
        begin match io with 
        | Input _ as io -> Ok (io, Unit)
        | Output expr -> 
          eval_expr expr _expr_env
          >>| fun value ->
          (Output value, value)
        end
        >>= fun (io, value) ->
        Ok ({ event with marking = { marking with value }; io } :: events))
      [] _spawn_events
    >>= fun _spawn_events ->

    (* Instantiate template instances present in the spawn *)
    (* FIXME: Maybe use instantiate_tmpls instead of this function *)
    let open Instantiation in
    { template_decls = program.template_decls
    ; events = []
    ; template_insts = _spawn_insts
    ; relations = [] 
    } |> instantiate ~expr_env 
    >>= fun ({ events = inst_spawn_events; template_insts = _; relations = inst_spawn_relations; _ }, _) -> 

    (* Put it all together *)
    Ok {
      program with 
      events = List.flatten [program.events; _spawn_events; inst_spawn_events];
      template_insts = [];
      relations = List.flatten [program.relations; _spawn_relations; inst_spawn_relations];
    }

  | ControlRelation (_from, guard, _dest, _op, _annot) -> 
    check_guard guard expr_env
    >>= fun _guard_value ->
    (* TODO: Check if the guard evaluates to True *)
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
    



(*
===============================================================
  Error messages
===============================================================
*)

and event_not_found event = Error Printf.(sprintf "Event %s not found" event)

and event_not_enabled event = 
  let (id, _) = event.info in
  Error Printf.(sprintf "Event %s is not enabled" id)

and invalid_guard_value value = 
  Error Printf.(sprintf "Invalid guard value %s" (string_of_expr value))

(*
===============================================================
  Available functions
===============================================================
*)

let rec execute ~event_id ?(expr = Unit) ?(event_env = empty_env) ?(expr_env = empty_env) program  = 
  (* 
    TODO: 
    - Find the event in the program [X]
    - Check if the event is enabled [+/-]
    - Execute the event
      - Update the event marking [X]
      - Execute the effects of the relations (propagate the relation effects) [+/-] (TESTING)
  *)
  (* preprocess_program program
  >>= fun (event_env, expr_env) -> *)

  match find_flat event_id event_env with
  | None -> event_not_found event_id
  | Some event -> 
    is_enabled event program (event_env, expr_env) 
    |> function
    | false -> event_not_enabled event
    | true -> Ok program 
    >>= fun program -> 
    begin match event.io with
      | Input _ -> execute_event event expr expr_env
      | Output data_expr -> execute_event event data_expr expr_env
    end
    >>= fun event ->
    propagate_effects event (event_env, expr_env) (update_event event program)
    

and preprocess_program ?(expr_env = empty_env) program = 
  fold_left_result
    (fun env event -> 
      let (id , _) = event.info in
      Ok (bind id event env))
    expr_env program.events
  >>= fun event_env ->
  (* print_endline "Event env:";
  print_endline (string_of_env string_of_event event_env); *)

  fold_left_result
    (fun env event -> 
      let (id, _) = event.info in
      Ok (bind id (record_event event) env))
    empty_env program.events
  >>= fun expr_env ->
  (* print_endline "Expr env:";
  print_endline (string_of_env string_of_expr expr_env); *)

  Ok (event_env, expr_env)

and execute_event event expr env = 
  eval_expr expr env
  >>= fun expr ->
  let marking = { event.marking with executed = true; value = expr } in
  let event = { event with marking } in
  Ok event

and view 
  ?(filter = (fun _ _ -> true)) 
  ?(event_env = empty_env) 
  ?(expr_env = empty_env) 
  ?(should_print_events = true)
  ?(should_print_relations = false)
  program = 
  (* preprocess_program program 
  >>= fun (event_env, expr_env) -> *)
  ( if should_print_events then
    List.filter (filter (event_env, expr_env)) program.events
    |> List.map (fun event -> string_of_event event)
    |> String.concat "\n"
  else "")
  |> fun events_str -> 
  ( if should_print_relations then 
      List.map (fun relation -> string_of_relation relation) program.relations
      |> String.concat "\n"
      |> Printf.sprintf "%s\n;\n%s" events_str
    else events_str )
  |> Result.ok

and view_debug program =
  view ~should_print_relations:true program

and view_enabled
  ?(event_env = empty_env)
  ?(expr_env = empty_env)
  ?(should_print_relations = false)  
  program =
  view ~event_env ~expr_env ~should_print_relations ~filter:(fun (event_env, expr_env) event -> 
    is_enabled event program (event_env, expr_env)) program

and _view_disabled program =
  view ~filter:(fun (event_env, expr_env) event -> 
    not (is_enabled event program (event_env, expr_env))) program
