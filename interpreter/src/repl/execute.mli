(** {1 Execute}
    @author Bruno Braga

    This module provides the implementation of the [execute] command in the REPL
    (Read-Eval-Print Loop) of the interpreter. The [execute] command is
    responsible for executing a specific event in the program and propagating
    its effects. It also handles errors that may occur during execution, such as
    invalid event IDs or type mismatches. The module includes functions to
    initialize the program, execute events, and handle command-line arguments.
*)

open Ast
open Error
open State
open Cmdliner

val term : (runtime_state -> (runtime_state, detailed_error list) result) Term.t
(** [term] is a command-line term for the [execute] command.
    @return the term handling the [execute] command. *)
