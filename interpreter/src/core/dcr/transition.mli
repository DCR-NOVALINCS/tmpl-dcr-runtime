open Ast
open Syntax
open Error
open Common
open Env

val is_enabled :
  event -> program -> event env * expr env -> (bool, detailed_error list) result
(** [is_enabled event program (event_env, expr_env)] checks if the event [event]
    is enabled in the program [program].
    @param event The event to check if it is enabled.
    @param program The program to check if the event is enabled.
    @param event_env The environment with the events.
    @param expr_env The environment with the expressions.
    @return result with a boolean indicating if the event is enabled. *)

val propagate_effects :
     event
  -> event env * expr env
  -> program
  -> (program * event env * expr env, detailed_error list) result
(** [propagate_effects event (event_env, expr_env) program] propagates the
    effects of the event [event] on the program [program].
    @param event The event to propagate the effects.
    @param event_env The environment with the events.
    @param expr_env The environment with the expressions.
    @param program The program to propagate the effects.
    @return result with the program with the effects propagated. *)
