open Syntax
open Misc.Monads
open Misc.Env
open Misc.Printing
open Evaluation
open Errors

(*
===============================================================
  Auxiliary functions
===============================================================
*)

let rec update_event event program = 
  let events = List.map (fun e -> 
    let (id, _) = e.data.info in
    let (id', _) = event.data.info in
    if id = id' then event else e) program.events in
  { program with events }

and is_enabled event program (event_env, expr_env) = 
  let relations = program.relations in
  let enabled = event.data.marking.data.included.data in
  List.fold_left 
    (fun enabled relation -> enabled && is_enabled_by relation event (event_env, expr_env))
    enabled relations

and is_enabled_by relation event (event_env, _expr_env) =
  let (id, _) = event.data.info in
  match relation.data with
  | SpawnRelation (_, _, _, _) -> true
  | ControlRelation (from, _guard, dest, _op, _annot) ->
    (* TODO: *)
    begin match find_flat dest.data event_env with
    | None -> true
    | Some dest_event when id = fst dest_event.data.info -> 
      begin match find_flat from.data event_env with
      | None -> true
      | Some from_event -> 
        let { marking = from_marking; _ } = from_event.data in
        let { marking = dest_marking; _ } = dest_event.data in
        begin match _op with 
        | Condition -> from_marking.data.included.data && from_marking.data.executed.data
                      && dest_marking.data.included.data
        | Milestone -> (not from_marking.data.pending.data) && from_marking.data.included.data
                      && dest_marking.data.included.data
        | _ -> true end
      end
    | _ -> true
    
    end

and check_guard guard env = 
  eval_expr guard env
  >>= fun guard_value ->
  match guard_value.data with
  | True -> Ok true
  | False -> Ok false
  | _ -> invalid_guard_value guard_value

and propagate_effects event (event_env, expr_env) program = 
  let relations = program.relations in
  fold_left_result
    (fun program relation -> 
      propagate_effect relation event (event_env, expr_env) program
      )  
    program relations

and propagate_effect relation event (event_env, expr_env) program = 
  let (id, _) = event.data.info in
  match relation.data with 
  | SpawnRelation (from, _guard, spawn_prog, _annot) -> 
    if not (from.data = id.data) then Ok program
    else
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
    let (spawn_events, spawn_insts, spawn_relations) = spawn_prog in

    (* Rename the event ids to new ones, to prevent id clashing *)
    fresh_event_ids spawn_events spawn_relations []
    >>= fun (spawn_events, spawn_relations) ->
      
    (* Begin new env scope and bind "@trigger" *)
    Ok (begin_scope expr_env)
    >>= fun expr_env ->
    Ok (bind "@trigger" (record_event event) expr_env)
    >>= fun expr_env ->
    (* print_endline "Expr env:";
    print_endline (string_of_env string_of_expr expr_env); *)

    (* Update values of the event inside of the spawn *)
    fold_left_result
      (fun events event -> 
        let { marking; io; _ } = event.data in 
        begin match io.data with 
        | Input _ -> Ok (io, marking.data.value)
        | Output expr -> 
          eval_expr expr expr_env
          >>= fun value ->
          Ok (annotate ~loc:io.loc ~ty:!(io.ty) (Output value), value)
        end
        >>= fun (io, value) ->
        let marking = { marking with data = { marking.data with value } } in
        Ok ({ event with data = { event.data with marking; io } } :: events))
      [] spawn_events
    >>= fun spawn_events ->

    (* Update values of the relations inside of the spawn *)

    (* Instantiate template instances present in the spawn *)
    (* FIXME: Maybe use instantiate_tmpls instead of this function *)
    let open Instantiation in
    { template_decls = program.template_decls
    ; events = []
    ; template_insts = spawn_insts
    ; relations = [] 
    } |> instantiate ~expr_env 
    >>= fun ({ events = inst_spawn_events; relations = inst_spawn_relations; _ }, _) -> 

    Logger.debug "Instantiated events from spawn:";
    Logger.debug @@ (List.map string_of_event inst_spawn_events |> String.concat "\n");
    Logger.debug "Instantiated relations from spawn:";
    Logger.debug @@ (List.map string_of_relation inst_spawn_relations |> String.concat "\n");

    (* Put it all together *)
    Ok {
      program with 
      events = List.flatten [program.events; spawn_events; inst_spawn_events];
      template_insts = [];
      relations = List.flatten [program.relations; spawn_relations; inst_spawn_relations];
    }

  | ControlRelation (from, guard, dest, op, _annot) -> 
    if not (from.data = id.data) then Ok program
    else
    check_guard guard expr_env
    >>= fun _guard_value ->
    (* TODO: Check if the guard evaluates to True *)
    (* Get the event [dest] *)
    (find_flat dest.data event_env |> Option.to_result ~none:(event_not_found dest.data |> Result.get_error) )
    >>= fun dest_event ->
    (* According to [op], apply the effect on the marking for both events *)
    (* Order of applying the relations: response -> exclude -> include -> other (do nothing)  *)
    let { marking = dest_marking; _ } = dest_event.data in
    let dest_marking = begin match op with
    | Response -> 
      { dest_marking with data = { dest_marking.data with pending = { dest_marking.data.pending with data = true} } } 
    | Exclude -> 
      { dest_marking with data = { dest_marking.data with included = { dest_marking.data.included with data = false} } } 
    | Include -> 
      { dest_marking with data = { dest_marking.data with included = { dest_marking.data.included with data = true} } } 
    | _ -> dest_marking
    end in
    let dest_event = { dest_event with data = { dest_event.data with marking = dest_marking } } in
    Ok (update_event dest_event program)

    (* | _ -> Ok program *)

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
    begin match event.data.io.data with
      | Input _ -> execute_event event expr expr_env
      | Output data_expr -> execute_event event data_expr.data expr_env
    end
    >>= fun event ->
    propagate_effects event (event_env, expr_env) (update_event event program)
    

and preprocess_program ?(expr_env = empty_env) program = 
  fold_left_result
    (fun env event -> 
      let (id , _) = event.data.info in
      Ok (bind id.data event env))
    expr_env program.events
  >>= fun event_env ->
  (* print_endline "Event env:";
  print_endline (string_of_env string_of_event event_env); *)

  fold_left_result
    (fun env event -> 
      let (id, _) = event.data.info in
      Ok (bind id.data (record_event event) env))
    empty_env program.events
  >>= fun expr_env ->
  (* print_endline "Expr env:";
  print_endline (string_of_env string_of_expr expr_env); *)

  Ok (event_env, expr_env)

and execute_event event expr env =
  let loc = match event.data.io.data with 
  | Input _ -> event.data.io.loc
  | Output expr -> expr.loc in 
  let expr = annotate ~loc expr in
  eval_expr expr env
  >>= fun expr ->
  let marking = { event.data.marking with data = { event.data.marking.data with value = expr; executed = annotate true } } in
  let event = { event with data = { event.data with marking } } in
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
    |> List.sort compare
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
