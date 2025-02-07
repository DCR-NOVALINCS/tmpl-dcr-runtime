open Ast
open Syntax
open Helper
open Common
open Monads.ResultMonad
open Env
open Printing

(* =============================================================================
   Enabledness functions
   ============================================================================= *)

let is_enabled event program (event_env, expr_env) =
  let enabled = is_included event in
  let id, _ = event.data.info in
  let* relations =
    find_all_relations ~filter:(fun _ _ dest -> dest.data = id.data) program
  in
  let is_enabled_by relation (event_env, expr_env) =
    match relation.data with
    | ControlRelation (from, guard, _, op) ->
        let* is_true = eval_bool guard expr_env in
        if not is_true then return true
        else
          let* from_event = find_event from event_env in
          let is_enabled =
            match op with
            | Condition -> is_included from_event && is_executed from_event
            | Milestone ->
                is_included from_event && is_executed from_event
                && not (is_pending event)
            | _ -> true
          in
          return is_enabled
    | _ -> return true
  in
  fold_left
    (fun enabled relation ->
      is_enabled_by relation (event_env, expr_env)
      >>| fun is_enabled_by_relation -> enabled && is_enabled_by_relation )
    enabled relations

(* =============================================================================
   Effect propagation functions
   ============================================================================= *)

and propagate_effects event (event_env, expr_env) program =
  let id, _ = event.data.info in
  let* relations =
    find_all_relations ~filter:(fun _ from _ -> from.data = id.data) program
  in
  let propagate_effect relation (event_env, expr_env) program =
    match relation.data with
    | SpawnRelation (_, guard, spawn_prog) ->
        let* is_true = eval_bool guard expr_env in
        if not is_true then return (program, event_env, expr_env)
        else
          (* Begin new env scope and bind trigger_id *)
          let event_env, expr_env =
            ( begin_scope event_env |> bind trigger_id event
            , begin_scope expr_env |> bind trigger_id (event_as_expr event) )
          in
          let* event_env, expr_env, spawn_prog =
            preprocess_subprogram ~event_env ~expr_env spawn_prog
          in
          (* Evaluate annotations from spawned elements *)
          let open Instantiation in
          (* Instantiate template instances present in the spawn *)
          Logger.debug "Instantiating template instances in spawn" ;
          let spawn_prog =
            {(to_program spawn_prog) with template_decls= program.template_decls}
          in
          let* ( { events= spawn_events
                 ; template_insts= spawn_insts
                 ; relations= spawn_relations
                 ; annotations= spawn_annots
                 ; _ }
               , event_env
               , expr_env ) =
            instantiate ~expr_env ~event_env spawn_prog
          in
          assert (List.length spawn_insts = 0 && List.length spawn_annots = 0) ;
          let* spawn_events =
            map (fun e -> update_event_io e expr_env) spawn_events
          in
          let* spawn_relations =
            map (fun r -> update_relation_guard r expr_env) spawn_relations
          in
          let event_env, expr_env = (end_scope event_env, end_scope expr_env) in
          (* Rename the event ids to new ones, to prevent id clashing *)
          let spawn_prog =
            (spawn_events, spawn_insts, spawn_relations, spawn_annots)
          in
          let* spawn_events, _, spawn_relations, _ = fresh_events spawn_prog in
          (* Put it all together *)
          return
            ( { program with
                events= List.flatten [spawn_events; program.events]
              ; template_insts= []
              ; relations= List.flatten [spawn_relations; program.relations]
              ; annotations= [] }
            , event_env
            , expr_env )
    | ControlRelation (_, guard, dest, op) ->
        let* is_true = eval_bool guard expr_env in
        if not is_true then return (program, event_env, expr_env)
        else
          let* dest_event = find_event dest event_env in
          let* dest_event =
            (* According to [op], apply the effect on the marking for both
               events *)
            (* Order of applying the relations: response -> exclude -> include ->
               other (do nothing) *)
            match op with
            | Response -> set_marking ~pending:true dest_event
            | Exclude -> set_marking ~included:false dest_event
            | Include -> set_marking ~included:true dest_event
            | _ -> return dest_event
          in
          return (update_event dest_event program, event_env, expr_env)
  in
  fold_left
    (fun (program, event_env, expr_env) relation ->
      propagate_effect relation (event_env, expr_env) program )
    (program, event_env, expr_env)
    relations
