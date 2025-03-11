(** {1 Execute command} *)

open Ast
open Error
open State
open Cmdliner

val term : (runtime_state -> (runtime_state, detailed_error list) result) Term.t
(** [term] is a command-line term for the [execute] command.
    @return the term handling the [execute] command. *)
