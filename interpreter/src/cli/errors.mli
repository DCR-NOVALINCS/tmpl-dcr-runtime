(** This module defines error-handling functions for the CLI (Command Line
    Interface) of the interpreter. Each function returns a result type that
    encapsulates either a successful value or a list of detailed errors. *)

open Ast
open Error

val invalid_logger_level : string -> ('a, detailed_error list) result
(** [invalid_logger_level level] is used to indicate that the provided logger
    level [level] is invalid.
    @param level The invalid logger level as a string.
    @return
      A result containing either a valid value or a list of detailed errors. *)

val invalid_command :
     ?errors:detailed_error list
  -> ?nearest:string
  -> ?distance:int
  -> string list
  -> ('a, detailed_error list) result
(** [invalid_command ?errors ?nearest ?distance commands] is used to indicate
    that the provided command [commands] is invalid.
    @param errors (optional) A list of detailed errors to include.
    @param nearest (optional) The nearest valid command as a suggestion.
    @param distance
      (optional) The edit distance between the invalid command and the nearest
      valid command.
    @param commands The invalid command as a list of strings.
    @return
      A result containing either a valid value or a list of detailed errors. *)

val file_not_exists :
  ?errors:detailed_error list -> string -> ('a, detailed_error list) result
(** [file_not_exists ?errors filepath] is used to indicate that the specified
    file [filepath] does not exist.
    @param errors (optional) A list of detailed errors to include.
    @param filepath The path to the file that does not exist.
    @return
      A result containing either a valid value or a list of detailed errors. *)

val invalid_file_extension :
     ?errors:detailed_error list
  -> supported:string
  -> ?got:string
  -> unit
  -> ('a, detailed_error list) result
(** [invalid_file_extension ?errors ~supported ?got ()] is used to indicate that
    the file has an unsupported extension.
    @param errors (optional) A list of detailed errors to include.
    @param supported The supported file extension(s) as a string.
    @param got (optional) The invalid file extension that was provided.
    @return
      A result containing either a valid value or a list of detailed errors. *)
