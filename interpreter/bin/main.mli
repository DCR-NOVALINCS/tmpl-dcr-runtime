(** {1 tmpl-dcr}

    This module provides the main entry point for the CLI. *)

open Templating
open Api
open Errors
open Cmdliner

(** [options] is a record type that contains all the options of the CLI.
    - [logger_level]: an optional string representing the logging level. *)
type options = {logger_level: string option}

val runtime : options -> string -> (runtime_state, detailed_error list) result
(** [runtime options filepath] is a function that takes an [options] record and
    a [filepath] as a string. It waits for user input, interprets the command,
    and returns a result.
    - [options]: the configuration options for the runtime.
    - [filepath]: the path to the file to be processed. *)

val runtime_cmd : (runtime_state, detailed_error list) result Cmd.t
(** [runtime_cmd] is a command that takes no arguments and returns a result. It
    encapsulates the runtime execution logic and error handling. *)
