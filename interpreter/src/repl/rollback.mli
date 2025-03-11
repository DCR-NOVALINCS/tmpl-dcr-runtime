(** {1 Rollback command} *)

open Cmdliner
open State
open Ast.Error

val term : (runtime_state -> (runtime_state, detailed_error list) result) Term.t
(** [term] is a command-line term for the [rollback] command.
    @return the term handling the [rollback] command. *)
