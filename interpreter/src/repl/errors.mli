(** This module defines a set of error-handling functions for the REPL
    (Read-Eval-Print Loop) of the interpreter. These functions return results
    that encapsulate potential errors in a structured way, allowing for detailed
    error reporting and handling. *)

open Ast
open Error

val invalid_command :
     ?errors:detailed_error list
  -> ?nearest:string
  -> ?distance:int
  -> string list
  -> ('a, detailed_error list) result
(** [invalid_command ?errors ?nearest ?distance commands] returns an error
    result indicating that the given command is invalid. Optionally, it can
    include a list of detailed errors, the nearest valid command, and the
    distance (e.g., Levenshtein distance) to the nearest valid command.

    @param errors Optional list of detailed errors.
    @param nearest Optional nearest valid command.
    @param distance Optional distance to the nearest valid command.
    @param commands The list of commands to validate.
    @return
      A result containing either a valid value or a list of detailed errors. *)

val file_not_exists :
  ?errors:detailed_error list -> string -> ('a, detailed_error list) result
(** [file_not_exists ?errors filepath] returns an error result indicating that
    the specified file does not exist.

    @param errors Optional list of detailed errors.
    @param filepath The path of the file to check.
    @return
      A result containing either a valid value or a list of detailed errors. *)

val invalid_file_extension :
     ?errors:detailed_error list
  -> supported:string
  -> ?got:string
  -> unit
  -> ('a, detailed_error list) result
(** [invalid_file_extension ?errors ~supported ?got ()] returns an error result
    indicating that the file has an invalid extension. It specifies the
    supported extensions and optionally the extension that was provided.

    @param errors Optional list of detailed errors.
    @param supported The supported file extensions.
    @param got Optional extension that was provided.
    @return
      A result containing either a valid value or a list of detailed errors. *)

val invalid_export_mode :
  ?errors:detailed_error list -> string -> ('a, detailed_error list) result
(** [invalid_export_mode ?errors mode] returns an error result indicating that
    the specified export mode is invalid.

    @param errors Optional list of detailed errors.
    @param mode The invalid export mode.
    @return
      A result containing either a valid value or a list of detailed errors. *)

val invalid_view_mode :
  ?errors:detailed_error list -> string -> ('a, detailed_error list) result
(** [invalid_view_mode ?errors mode] returns an error result indicating that the
    specified view mode is invalid.

    @param errors Optional list of detailed errors.
    @param mode The invalid view mode.
    @return
      A result containing either a valid value or a list of detailed errors. *)

val invalid_view_combination :
     ?errors:detailed_error list
  -> ?hint:string
  -> string
  -> string
  -> ('a, detailed_error list) result
(** [invalid_view_combination ?errors ?hint view1 view2] returns an error result
    indicating that the combination of the two specified view modes is invalid.
    Optionally, it can include a hint for resolving the issue.

    @param errors Optional list of detailed errors.
    @param hint Optional hint for resolving the issue.
    @param view1 The first view mode.
    @param view2 The second view mode.
    @return
      A result containing either a valid value or a list of detailed errors. *)

val invalid_number_rollback :
  ?errors:detailed_error list -> int -> ('a, detailed_error list) result
(** [invalid_number_rollback ?errors n] returns an error result indicating that
    the specified number of rollbacks is invalid.

    @param errors Optional list of detailed errors.
    @param n The invalid number of rollbacks.
    @return
      A result containing either a valid value or a list of detailed errors. *)
