(** {1 Lexing and Parsing}

    This module provides functions to parse a lexbuf into a valid program or
    expression. *)

open Ast
open Syntax
open Error

val parse_program :
  ?filename:string -> Lexing.lexbuf -> (program, detailed_error list) result
(** [parse_program ?filename lexbuf] parses the lexbuf as a program and returns
    the result. This function sets the [pos_fname] field in the lexbuf to the
    provided filename. If an error occurs, it returns the error.
    @param filename The name of the file being parsed.
    @param lexbuf The lexbuf to be parsed.
    @return The result of the parsing as result. *)

val parse_expression :
  ?filename:string -> Lexing.lexbuf -> (expr, detailed_error list) result
(** [parse_expression ?filename lexbuf] parses the lexbuf as an expression and
    returns the result. This function sets the [pos_fname] field in the lexbuf
    to the provided filename. If an error occurs, it returns the error.
    @param filename The name of the file being parsed.
    @param lexbuf The lexbuf to be parsed.
    @return The result of the parsing as result. *)
