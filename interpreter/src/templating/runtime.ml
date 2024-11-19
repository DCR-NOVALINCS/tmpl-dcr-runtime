open Syntax
open Evaluation
open Errors
open Program_helper
open Misc.Monads.ResultMonad
open Misc.Env
(* open Misc.Printing *)

(* =============================================================================
   Enabledness functions
   ============================================================================= *)

let rec is_enabled event program (event_env, expr_env) =
  let relations = program.relations in
  let enabled = event.data.marking.data.included.data in
  fold_left
    (fun enabled relation ->
      is_enabled_by relation event (event_env, expr_env)
      >>= fun is_enabled_by_relation ->
      return (enabled && is_enabled_by_relation) )
    enabled relations
(* List.fold_left (fun enabled relation -> enabled && is_enabled_by relation
   event (event_env, expr_env) ) enabled program.relations *)

and is_enabled_by relation event (event_env, expr_env) =
  let id, _ = event.data.info in
  match relation.data with
  | SpawnRelation _ -> return true
  | ControlRelation (from, guard, dest, op, _annot) -> (
      if not (id.data = dest.data) then return true
      else
        (* Check the value of the guard, if it evaluates to false, the event is
           enabled regardless of the type of the relations *)
        check_guard guard expr_env
        >>= fun is_guard_check ->
        if not is_guard_check then return true
        else
          (* Get the event that is on the left side *)
          match find_flat from.data event_env with
          | None -> return true
          | Some from_event -> (
              (* Check the enabledness of the event, according the selected
                 events and type of relation *)
              let {marking= from_marking; _} = from_event.data in
              let {marking= dest_marking; _} = event.data in
              match op with
              | Condition ->
                  return
                    ( from_marking.data.included.data
                    && from_marking.data.executed.data
                    && dest_marking.data.included.data )
              | Milestone ->
                  return
                    ( (not from_marking.data.pending.data)
                    && from_marking.data.included.data
                    && dest_marking.data.included.data )
              | _ -> return true ) )

(** [check_guard guard env] checks if the [guard] is true with the environment
    [env].
    @param guard The guard to be checked.
    @param env The environment to be used for evaluation.
    @return result with [true] if the guard is true, [false] otherwise. *)
and check_guard guard env =
  eval_expr guard env
  >>= fun guard_value ->
  match guard_value.data with
  | True -> return true
  | False -> return false
  | _ -> (
    match !(guard_value.ty) with
    | Some ty -> type_mismatch [BoolTy] [ty]
    | _ ->
        should_not_happen ~module_path:"runtime.ml"
          "The type of the guard is missing" )

(* =============================================================================
   Effect propagation functions
   ============================================================================= *)

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
      if not (from.data = id.data) then return program
      else
        check_guard guard expr_env
        >>= fun is_guard_true ->
        if not is_guard_true then return program
        else
          (* FIXME: Alpha rename correctly the events!! *)
          let spawn_events, spawn_insts, spawn_relations = spawn_prog in
          (* Rename the event ids to new ones, to prevent id clashing *)
          fresh_event_ids spawn_events spawn_relations []
          >>= fun (spawn_events, spawn_relations) ->
          (* Begin new env scope and bind "@trigger" *)
          return (begin_scope expr_env)
          >>= fun expr_env ->
          return (begin_scope event_env)
          >>= fun event_env ->
          return (bind "@trigger" (event_as_expr event) expr_env)
          >>= fun expr_env ->
          return (bind "@trigger" event event_env)
          >>= fun event_env ->
          (* Update values of the event inside of the spawn *)
          map (fun event -> update_event_value event expr_env) spawn_events
          >>= fun spawn_events ->
          (* Evaluate annotions from spawned elements *)
          let open Instantiation in
          evaluate_annotations_of_subprogram
            (spawn_events, spawn_insts, spawn_relations)
            (expr_env, event_env, empty_env)
          >>= fun (spawn_events, spawn_insts, spawn_relations) ->
          (* fold_left (fun events event -> let { marking; io; _ } = event.data
             in begin match io.data with | Input _ -> return (io,
             marking.data.value) | Output expr -> eval_expr expr expr_env >>=
             fun value -> return (annotate ~loc:io.loc ~ty:!(io.ty) (Output
             value), value) end >>= fun (io, value) -> let marking = { marking
             with data = { marking.data with value } } in return ({ event with
             data = { event.data with marking; io } } :: events)) []
             spawn_events >>= fun spawn_events -> *)

          (* Instantiate template instances present in the spawn *)
          (* FIXME: Maybe use instantiate_tmpls instead of this function *)
          { template_decls= program.template_decls
          ; events= []
          ; template_insts= spawn_insts
          ; relations= [] }
          |> instantiate ~expr_env ~event_env
          >>= fun ( { events= inst_spawn_events
                    ; relations= inst_spawn_relations
                    ; _ }
                  , _ ) ->
          (* Logger.debug "Instantiated events from spawn:" ; Logger.debug @@
             Unparser.PlainUnparser.unparse_events inst_spawn_events ;
             Logger.debug "Instantiated relations from spawn:" ; Logger.debug @@
             Unparser.PlainUnparser.unparse_relations inst_spawn_relations ; *)
          (* Put it all together *)
          return
            { program with
              events=
                List.flatten [program.events; inst_spawn_events; spawn_events]
            ; template_insts= []
            ; relations=
                List.flatten
                  [program.relations; inst_spawn_relations; spawn_relations] }
  | ControlRelation (from, guard, dest, op, _annot) ->
      if not (from.data = id.data) then return program
      else
        check_guard guard expr_env
        >>= fun is_guard_true ->
        if not is_guard_true then return program
        else
          (* Get the event [dest] *)
          find_flat dest.data event_env
          |> Option.to_result
               ~none:(event_not_found dest.data |> Result.get_error)
          >>= fun dest_event ->
          (* According to [op], apply the effect on the marking for both
             events *)
          (* Order of applying the relations: response -> exclude -> include ->
             other (do nothing) *)
          ( match op with
          | Response ->
              set_marking ~marking:(mk_marking ~pending:true ()) dest_event
          | Exclude ->
              set_marking ~marking:(mk_marking ~included:false ()) dest_event
          | Include ->
              set_marking ~marking:(mk_marking ~included:true ()) dest_event
          | _ -> return dest_event )
          >>= fun dest_event -> return (update_event dest_event program)

(* | _ -> return program *)
