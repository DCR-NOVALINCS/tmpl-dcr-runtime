open State
open Cmdliner

(** [quit_cmd state] is a command that quits the REPL. It exits the REPL by
    returning 0 and flushing all output buffers.
    @param state is the current runtime state.
    @return the new runtime state. *)
let quit_cmd (_ : runtime_state) = exit 0

let term = Term.(const quit_cmd)
