(** {1 Debug command} *)

open State
open Cmdliner

val term : (runtime_state -> (runtime_state, 'a) result) Term.t
(** [term] is a command-line term for the {b debug} command, which prints the
    current state of the interpreter.
    @return the command-line term for the {b debug} command. *)
