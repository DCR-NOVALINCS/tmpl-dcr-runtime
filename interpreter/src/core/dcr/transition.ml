open Ast
open Syntax
open Evaluation
open Helper
open Common
open Monads.ResultMonad
open Env

(* =============================================================================
   Enabledness functions
   ============================================================================= *)

let is_enabled event program (event_env, expr_env) =
  let relations = program.relations in
  let enabled = is_included event in
  let id, _ = event.data.info in
  let is_enabled_by relation (event_env, expr_env) =
    match relation.data with
    | SpawnRelation _ -> return true
    | ControlRelation (from, guard, dest, op) -> (
        if not (id.data = dest.data) then return true
        else
          eval_bool guard expr_env
          >>= fun is_guard_true ->
          if not is_guard_true then return true
          else
            match find_flat from.data event_env with
            | None -> return true
            | Some from_event -> (
              match op with
              | Condition ->
                  return
                    ( is_included from_event && is_executed from_event
                    && is_included event )
              | Milestone ->
                  return
                    ( is_included from_event && is_executed from_event
                    && not (is_pending event) )
              | _ -> return true ) )
  in
  fold_left
    (fun enabled relation ->
      is_enabled_by relation (event_env, expr_env)
      >>| fun is_enabled_by_relation -> enabled && is_enabled_by_relation )
    enabled relations

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
  let id, _ = event.data.info in
  let propagate_effect relation (event_env, expr_env) program =
    match relation.data with
    | SpawnRelation (from, guard, spawn_prog) ->
        if not (from.data = id.data) then return (program, event_env, expr_env)
        else
          eval_bool guard expr_env
          >>= fun is_guard_true ->
          if not is_guard_true then return (program, event_env, expr_env)
          else
            (* Begin new env scope and bind trigger_id *)
            return (begin_scope event_env, begin_scope expr_env)
            >>= fun (event_env, expr_env) ->
            return
              ( bind trigger_id event event_env
              , bind trigger_id (event_as_expr event) expr_env )
            >>= fun (event_env, expr_env) ->
            preprocess_subprogram ~event_env ~expr_env spawn_prog
            >>= fun ( event_env
                    , expr_env
                    , (spawn_events, spawn_insts, spawn_relations, spawn_annots)
                    ) ->
            (* Evaluate annotations from spawned elements *)
            let open Instantiation in
            (* Instantiate template instances present in the spawn *)
            mk_program ~template_decls:program.template_decls
              ~events:spawn_events ~relations:spawn_relations
              ~annotations:spawn_annots ~template_insts:spawn_insts ()
            |> instantiate ~expr_env ~event_env
            >>= fun ( {events= spawn_events; relations= spawn_relations; _}
                    , event_env
                    , expr_env ) ->
            (* Update event values *)
            map
              (fun e -> update_event_io ~eval:eval_expr e expr_env)
              spawn_events
            >>= fun spawn_events ->
            (* Update relations *)
            map (fun r -> update_relation_guard r expr_env) spawn_relations
            >>= fun spawn_relations ->
            return (end_scope event_env, end_scope expr_env)
            >>= fun (event_env, expr_env) ->
            preprocess_subprogram ~event_env ~expr_env
              (spawn_events, [], spawn_relations, [])
            >>= fun ( event_env
                    , expr_env
                    , (spawn_events, spawn_insts, spawn_relations, spawn_annots)
                    ) ->
            (* Rename the event ids to new ones, to prevent id clashing *)
            fresh_event_ids
              (spawn_events, spawn_insts, spawn_relations, spawn_annots)
            >>= fun (spawn_events, _spawn_insts, spawn_relations, _spawn_annots) ->
            (* Put it all together *)
            return
              ( { program with
                  events= List.flatten [spawn_events; program.events]
                ; template_insts= []
                ; relations= List.flatten [spawn_relations; program.relations]
                ; annotations= [] }
              , event_env
              , expr_env )
    | ControlRelation (from, guard, dest, op) ->
        if not (from.data = id.data) then return (program, event_env, expr_env)
        else
          eval_bool guard expr_env
          >>= fun is_guard_true ->
          if not is_guard_true then return (program, event_env, expr_env)
          else
            find_event dest event_env
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
  in
  fold_left
    (fun (program, event_env, expr_env) relation ->
      propagate_effect relation (event_env, expr_env) program )
    (program, event_env, expr_env)
    relations

(* | _ -> return program *)
