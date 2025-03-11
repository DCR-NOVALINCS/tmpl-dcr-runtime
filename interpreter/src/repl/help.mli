(** {1 Help command} *)

open State
open Cmdliner

val term :
  (string * cmd) list -> (runtime_state -> (runtime_state, 'a) result) Term.t
(** [term] is a command-line term for the [help] command.
    @return the term handling the [help] command. *)
