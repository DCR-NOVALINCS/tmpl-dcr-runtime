(** {1 View command} *)

open Cmdliner
open State

val term : (runtime_state -> (runtime_state, 'a) result) Term.t
(** [term] is a command-line term for the {b view} command, which prints the
    current state of the interpreter.
    @return the command-line term for the {b view} command. *)
