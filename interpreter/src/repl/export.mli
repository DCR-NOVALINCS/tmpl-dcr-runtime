(** {1 Export command} *)

open Cmdliner
open State
open Ast
open Error

val term : (runtime_state -> (runtime_state, detailed_error list) result) Term.t
(** [term] is a command-line term for the [export] command.
    @return the term handling the [export] command. *)
