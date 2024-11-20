(* open Syntax *)
open Errors
open Misc.Env
open Misc.Monads.ResultMonad

(* =============================================================================
   Entry point
   ============================================================================= *)

(* Typecheck the program *)
(* - [] Check the template definitions\ *)
(* - Check the events *)
(* - [] Check the expr inside of the event's marking and value *)
(* - [] Check the instances *)
(* - Check the relations *)
(* - [] Check the [from] and [to] events *)
(* - [] Check the [guard] *)
(* - If the relation is a [spawn] relation: *)
(* - [] *)
let typecheck ?(_expr_env = empty_env) ?(_event_env = empty_env) _program =
  todo "typecheck template defs"
  >>= fun _ ->
  todo "typecheck events"
  >>= fun _ ->
  todo "typecheck insts"
  >>= fun _ -> todo "typecheck relations" >>= fun _ -> return true
