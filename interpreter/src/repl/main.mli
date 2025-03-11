(** {1 Main REPL module}

    This module is the entry point for the REPL. This module contains all the
    commands available for the interpreter to execute.

    All the commands are defined in separate modules in the [commands] directory
    and are loaded dynamically in the variable [commands] in this module. This
    variable yields a the tuple
    [((string * State.cmd) list * string Common.Bktree.t)] where the first
    element is a list of tuples with the name of the command and the command
    itself, and the second element is a {m Common.Bktree} with the names of the
    commands for fast lookup of the nearest command in case of a typo.

    Each command is a module that must implement the following functions:
    {[
      val term : (State.runtime_state -> 'a) Term.t

      val cmd : t_a1 -> t_a2 -> ... -> t_an -> State.runtime_state -> (State.runtime_state, detailed_error list) result

      val a1 : t_a1 Term.t

      val a2 : t_a2 Term.t

      (* ... *)

      val an : t_an Term.t
    ]}
    where [t_a1], [t_a2], ..., [t_an] are the types of the arguments of the
    command. The [term] function is the entry point for the execution of the
    command, taking the current runtime state and returning the new runtime
    state. The [cmd] function is the actual implementation of the command,
    taking the arguments of the command and the current runtime state, and
    returning the new runtime state. The [a1], [a2], ..., [an] functions are the
    command-line terms for the arguments of the command. *)

open State
open Ast
open Error

val runtime : string -> (runtime_state, detailed_error list) result
(** [runtime filename] is the runtime state of the interpreter after executing
    the commands in the file [filename].
    @param filename the name of the file containing the commands to execute.
    @return
      the runtime state of the interpreter after executing the commands in the
      file. *)
