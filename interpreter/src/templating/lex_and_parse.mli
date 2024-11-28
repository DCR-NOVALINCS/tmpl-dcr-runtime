open Errors
open Syntax

val parse_with_error_handling :
     ?filepath:string
  -> Lexing.lexbuf
  -> (Lexing.lexbuf -> 'a)
  -> ('a, detailed_error list) result
(** [parse_with_error_handling ?filepath lexbuf parse_fun] parses the lexbuf
    using the parse_fun and returns the result. If an error occurs, it returns
    the error.
    @param filepath The path of the file being parsed.
    @param lexbuf The lexbuf to be parsed.
    @param parse_fun The function to parse the lexbuf.
    @return The result of the parsing as result. *)

val parse_program :
  ?filename:string -> Lexing.lexbuf -> (program, detailed_error list) result
(** [parse_program ?filename lexbuf] parses the lexbuf as a program and returns
    the result. If an error occurs, it returns the error.
    @param filename The name of the file being parsed.
    @param lexbuf The lexbuf to be parsed.
    @return The result of the parsing as result. *)

val parse_expression :
  ?filename:string -> Lexing.lexbuf -> (expr, detailed_error list) result
(** [parse_expression ?filename lexbuf] parses the lexbuf as an expression and
    returns the result. If an error occurs, it returns the error.
    @param filename The name of the file being parsed.
    @param lexbuf The lexbuf to be parsed.
    @return The result of the parsing as result. *)
