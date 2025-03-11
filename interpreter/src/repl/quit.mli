(** {3 Quit command} *)

open Cmdliner

val term : (State.runtime_state -> 'a) Term.t
(** [term] is a command-line term for the quit command, which exits the REPL by
    returning 0 and flushing all output buffers.
    @return the command-line term for the quit command. *)
