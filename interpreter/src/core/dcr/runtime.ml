open Ast
open Syntax
open Error
open Evaluation

(* open Errors *)
open Helper
open Common
open Monads.ResultMonad
open Env
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
      >>| fun is_enabled_by_relation -> enabled && is_enabled_by_relation )
    enabled relations

and is_enabled_by relation event (event_env, expr_env) =
  let id, _ = event.data.info in
  match relation.data with
  | SpawnRelation _ -> return true
  | ControlRelation (from, guard, dest, op) -> (
      if not (id.data = dest.data) then return true
      else
        (* Check the value of the guard, if it evaluates to false, the event is
           enabled regardless of the type of the relations *)
        check_guard guard expr_env
        >>= fun is_guard_check ->
        if not is_guard_check then return true
        else
          (* Get the event that is on the left side *)
          (* FIXME: Update event_env when altering any event marking *)
          match find_flat from.data event_env with
          | None -> return true
          | Some from_event -> (
              (* Check the enabledness of the event, according the selected
                 events and type of relation *)
              let {marking= {data= from_marking; _}; _} = from_event.data in
              let {marking= {data= dest_marking; _}; _} = event.data in
              match op with
              | Condition ->
                  return
                    ( from_marking.included.data && from_marking.executed.data
                    && dest_marking.included.data )
              | Milestone ->
                  return
                    ( (not from_marking.pending.data)
                    && from_marking.included.data && dest_marking.included.data
                    )
              | _ -> return true ) )

(** [check_guard guard expr_env] checks if the [guard] is true with the
    environment of values [expr_env].
    @param guard The guard to be checked.
    @param expr_env The environment of values to be used for evaluation.
    @return result with {b true} if the guard is true, {b false} otherwise. *)
and check_guard guard expr_env =
  eval_expr guard expr_env
  >>= fun guard_value ->
  match guard_value.data with
  | True -> return true
  | False -> return false
  | _ ->
      should_not_happen ~module_path:"runtime.ml"
        "Expecting boolean in the guard expression"

(* =============================================================================
   Effect propagation functions
   ============================================================================= *)

(** [propagate_effects event (event_env, expr_env) program] propagates the
    effects of the event [event] on the program [program].
    @param event The event to propagate the effects.
    @param event_env The environment with the events.
    @param expr_env The environment with the expressions.
    @param program The program to propagate the effects.
    @return result with the program with the effects propagated. *)
and propagate_effects event (event_env, expr_env) program =
  let relations = program.relations in
  fold_left
    (fun (program, event_env, expr_env) relation ->
      propagate_effect relation event (event_env, expr_env) program )
    (program, event_env, expr_env)
    relations

and propagate_effect relation event (event_env, expr_env) program =
  let id, _ = event.data.info in
  match relation.data with
  | SpawnRelation (from, guard, spawn_prog) ->
      if not (from.data = id.data) then return (program, event_env, expr_env)
      else
        check_guard guard expr_env
        >>= fun is_guard_true ->
        if not is_guard_true then return (program, event_env, expr_env)
        else
          let spawn_events, spawn_insts, spawn_relations, spawn_annots =
            spawn_prog
          in
          (* Begin new env scope and bind trigger_id *)
          return (begin_scope event_env, begin_scope expr_env)
          >>= fun (event_env, expr_env) ->
          return
            ( bind trigger_id event event_env
            , bind trigger_id (event_as_expr event) expr_env )
          >>= fun (event_env, expr_env) ->
          (* Evaluate annotations from spawned elements *)
          let open Instantiation in
          (* Instantiate template instances present in the spawn *)
          mk_program ~template_decls:program.template_decls
            ~annotations:spawn_annots ~template_insts:spawn_insts ()
          |> instantiate ~expr_env ~event_env
          >>= fun ( { events= inst_spawn_events
                    ; relations= inst_spawn_relations
                    ; _ }
                  , event_env
                  , expr_env ) ->
          (* Rename the event ids to new ones, to prevent id clashing *)
          (* FIXME: Change alpha renaming position!!! *)
          let spawn_events, spawn_relations =
            ( List.flatten [inst_spawn_events; spawn_events]
            , List.flatten [inst_spawn_relations; spawn_relations] )
          in
          fresh_event_ids spawn_events spawn_relations
          >>= fun (spawn_events, spawn_relations) ->
          (* Update event values *)
          map (fun e -> update_event_io ~eval:eval_expr e expr_env) spawn_events
          >>= fun spawn_events ->
          (* Update relations *)
          map (fun r -> update_relation_guard r expr_env) spawn_relations
          >>= fun spawn_relations ->
          (* Put it all together *)
          return
            ( { program with
                events= List.flatten [spawn_events; program.events]
              ; template_insts= []
              ; relations= List.flatten [spawn_relations; program.relations] }
            , end_scope event_env
            , end_scope expr_env )
  | ControlRelation (from, guard, dest, op) ->
      if not (from.data = id.data) then return (program, event_env, expr_env)
      else
        check_guard guard expr_env
        >>= fun is_guard_true ->
        if not is_guard_true then return (program, event_env, expr_env)
        else
          (* Get the event [dest] *)
          find_flat dest.data event_env
          |> Option.to_result
               ~none:
                 (event_not_found ~loc:dest.loc dest.data |> Result.get_error)
          >>= fun dest_event ->
          (* According to [op], apply the effect on the marking for both
             events *)
          (* Order of applying the relations: response -> exclude -> include ->
             other (do nothing) *)
          ( match op with
          | Response -> set_marking ~pending:true dest_event
          | Exclude -> set_marking ~included:false dest_event
          | Include -> set_marking ~included:true dest_event
          | _ -> return dest_event )
          >>= fun dest_event ->
          return (update_event dest_event program, event_env, expr_env)

(* | _ -> return program *)
