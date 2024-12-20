open Ast
open Error
open Lexing

val lexing_error :
     ?errors:detailed_error list
  -> lexbuf
  -> string
  -> ('a, detailed_error list) result
(** [lexing_error ?errors lexbuf message] is a parser error that occurs during
    lexing. It contains the location of the error, a message describing the
    error, and a hint to help the user fix the error. *)

val syntax_error :
  ?errors:detailed_error list -> lexbuf -> ('a, detailed_error list) result
(** [syntax_error ?errors lexbuf] is a parser error that occurs during parsing.
    It contains the location of the error and the token that caused the error. *)

val unexpected_eof :
  ?errors:detailed_error list -> lexbuf -> ('a, detailed_error list) result
(** [unexpected_eof ?errors lexbuf] is a parser error that occurs when the
    parser encounters an unexpected end of file. It contains the location of the
    error and a hint to help the user fix the error. *)

val unknown_error :
  ?errors:detailed_error list -> lexbuf -> ('a, detailed_error list) result
(** [unknown_error ?errors lexbuf] is a parser error that occurs when the parser
    encounters an unknown error. It contains the location of the error and a
    hint to help the user fix the error. When this error occurs, it is
    recommended to report the issue in the repository. *)
