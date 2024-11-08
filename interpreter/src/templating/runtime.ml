open Syntax
open Evaluation
open Errors
open Misc.Monads.ResultMonad
open Misc.Env
open Misc.Printing

(* =============================================================== Updating
   event functions
   =============================================================== *)

(** [update_event event program] updates the [event] in the [program]
    by replacing the old event with the new one.
    @param event The new event to be updated.
    @param program The program where the event is to be updated.
    @return The updated program. *)
let rec update_event event program =
  let events =
    List.map
      (fun e ->
        let id, _ = e.data.info in
        let id', _ = event.data.info in
        if id = id' then event else e )
      program.events
  in
  {program with events}

and update_event_value event expr_env =
  let {marking; io; _} = event.data in
  ( match io.data with
  | Input ty ->
      eval_expr marking.data.value expr_env
      >>= fun value ->
      Ok (annotate ~loc:io.loc ~ty:!(io.ty) (Input ty), value)
  | Output expr ->
      eval_expr expr expr_env
      >>= fun value ->
      Ok (annotate ~loc:io.loc ~ty:!(io.ty) (Output value), value) )
  >>= fun (io, value) ->
  let marking = {marking with data= {marking.data with value}} in
  Ok {event with data= {event.data with marking; io}}

and set_marking ?(marking = default_marking) event =
  let {marking= prev_marking; _} = event.data in
  Ok
    { event with
      data= {event.data with marking= {prev_marking with data= marking}} }

(* ===============================================================
   Enabledness functions
   =============================================================== *)

(** [is_enabled] checks if the [event] is enabled in the [program] by
    checking all the relations in the program, given the current environment.
    @param event The event to be checked.
    @param program The program where the event is to be checked.
    @param env The environment to be used for evaluation.
    @return [true] if the event is enabled, [false] otherwise. *)
and is_enabled event program (event_env, expr_env) =
  let relations = program.relations in
  let enabled = event.data.marking.data.included.data in
  List.fold_left
    (fun enabled relation ->
      enabled && is_enabled_by relation event (event_env, expr_env) )
    enabled relations

(** [is_enabled_by] checks if the [event] is enabled by the [relation] in the [program]
    by checking the relation's guard and the marking of the events.
    @param relation The relation to be checked.
    @param event The event to be checked.
    @param env The environment to be used for evaluation.
    @return
      [true] if the event is enabled by the relation, [false] otherwise. *)
and is_enabled_by relation event (event_env, _expr_env) =
  let id, _ = event.data.info in
  match relation.data with
  | SpawnRelation (_, _, _, _) -> true
  | ControlRelation (from, _guard, dest, _op, _annot) -> (
    (* TODO: Check guards!! *)
    match find_flat dest.data event_env with
    | None -> true
    | Some dest_event when id = fst dest_event.data.info -> (
      match find_flat from.data event_env with
      | None -> true
      | Some from_event -> (
          let {marking= from_marking; _} = from_event.data in
          let {marking= dest_marking; _} = dest_event.data in
          match _op with
          | Condition ->
              from_marking.data.included.data
              && from_marking.data.executed.data
              && dest_marking.data.included.data
          | Milestone ->
              (not from_marking.data.pending.data)
              && from_marking.data.included.data
              && dest_marking.data.included.data
          | _ -> true ) )
    | _ -> true )

(** [check_guard guard env] checks if the [guard] is true with the environment [env].
    @param guard The guard to be checked.
    @param env The environment to be used for evaluation.
    @return result with [true] if the guard is true, [false] otherwise. *)
and check_guard guard env =
  eval_expr guard env
  >>= fun guard_value ->
  match guard_value.data with
  | True -> Ok true
  | False -> Ok false
  | _ -> invalid_guard_value guard_value

(* =============================================================== Effect
   propagation functions
   =============================================================== *)

and propagate_effects event (event_env, expr_env) program =
  let relations = program.relations in
  fold_left
    (fun program relation ->
      propagate_effect relation event (event_env, expr_env) program )
    program relations

and propagate_effect relation event (event_env, expr_env) program =
  let id, _ = event.data.info in
  match relation.data with
  | SpawnRelation (from, guard, spawn_prog, _annot) ->
      if not (from.data = id.data) then Ok program
      else
        check_guard guard expr_env
        >>= fun is_guard_true ->
        if not is_guard_true then Ok program
        else
          (* FIXME: Alpha rename correctly the events!! *)
          let spawn_events, spawn_insts, spawn_relations = spawn_prog in
          (* Rename the event ids to new ones, to prevent id clashing *)
          fresh_event_ids spawn_events spawn_relations []
          >>= fun (spawn_events, spawn_relations) ->
          (* Begin new env scope and bind "@trigger" *)
          Ok (begin_scope expr_env)
          >>= fun expr_env ->
          Ok (bind "@trigger" (record_event event) expr_env)
          >>= fun expr_env ->
          (* Update values of the event inside of the spawn *)
          map (fun event -> update_event_value event expr_env) spawn_events
          >>= fun spawn_events ->
          (* fold_left (fun events event -> let { marking; io; _ } =
             event.data in begin match io.data with | Input _ -> Ok (io,
             marking.data.value) | Output expr -> eval_expr expr expr_env
             >>= fun value -> Ok (annotate ~loc:io.loc ~ty:!(io.ty) (Output
             value), value) end >>= fun (io, value) -> let marking = {
             marking with data = { marking.data with value } } in Ok ({
             event with data = { event.data with marking; io } } ::
             events)) [] spawn_events >>= fun spawn_events -> *)

          (* Update values of the relations inside of the spawn *)

          (* Instantiate template instances present in the spawn *)
          (* FIXME: Maybe use instantiate_tmpls instead of this function *)
          let open Instantiation in
          { template_decls= program.template_decls
          ; events= []
          ; template_insts= spawn_insts
          ; relations= [] }
          |> instantiate ~expr_env
          >>= fun ( { events= inst_spawn_events
                    ; relations= inst_spawn_relations
                    ; _ }
                  , _ ) ->
          Logger.debug "Instantiated events from spawn:" ;
          Logger.debug
          @@ Unparser.PlainUnparser.unparse_events inst_spawn_events ;
          Logger.debug "Instantiated relations from spawn:" ;
          Logger.debug
          @@ Unparser.PlainUnparser.unparse_relations inst_spawn_relations ;
          (* Put it all together *)
          Ok
            { program with
              events=
                List.flatten
                  [program.events; inst_spawn_events; spawn_events]
            ; template_insts= []
            ; relations=
                List.flatten
                  [program.relations; inst_spawn_relations; spawn_relations]
            }
  | ControlRelation (from, guard, dest, op, _annot) ->
      if not (from.data = id.data) then Ok program
      else
        check_guard guard expr_env
        >>= fun is_guard_true ->
        if not is_guard_true then Ok program
        else
          (* Get the event [dest] *)
          find_flat dest.data event_env
          |> Option.to_result
               ~none:(event_not_found dest.data |> Result.get_error)
          >>= fun dest_event ->
          (* According to [op], apply the effect on the marking for both
             events *)
          (* Order of applying the relations: response -> exclude ->
             include -> other (do nothing) *)
          let {marking= dest_marking; _} = dest_event.data in
          ( match op with
          | Response ->
              set_marking
                ~marking:
                  (mk_marking ~executed:dest_marking.data.executed.data
                     ~pending:true
                     ~included:dest_marking.data.included.data
                     ~value:dest_marking.data.value.data () )
                dest_event
              (* { dest_marking with data = { dest_marking.data with
                 pending = { dest_marking.data.pending with data = true} }
                 } *)
          | Exclude ->
              set_marking
                ~marking:
                  (mk_marking ~executed:dest_marking.data.executed.data
                     ~pending:dest_marking.data.pending.data
                     ~included:false ~value:dest_marking.data.value.data () )
                dest_event
              (* { dest_marking with data = { dest_marking.data with
                 included = { dest_marking.data.included with data = false}
                 } } *)
          | Include ->
              set_marking
                ~marking:
                  (mk_marking ~executed:dest_marking.data.executed.data
                     ~pending:dest_marking.data.pending.data ~included:true
                     ~value:dest_marking.data.value.data () )
                dest_event
              (* { dest_marking with data = { dest_marking.data with
                 included = { dest_marking.data.included with data = true}
                 } } *)
          | _ -> Ok dest_event )
          >>= fun dest_event -> Ok (update_event dest_event program)

(* | _ -> Ok program *)
