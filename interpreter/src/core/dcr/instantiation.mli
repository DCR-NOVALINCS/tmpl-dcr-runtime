open Ast
open Syntax
open Error
open Common
open Env

val instantiate :
     ?expr_env:expr' annotated env
  -> ?event_env:event' annotated env
  -> program
  -> ( program * event' annotated env * expr' annotated env
     , detailed_error list )
     result
(** [instantiate ?expr_env ?event_env program] instantiates the templates and
    the meta-programming operators present in [program].

    @param expr_env The environment of expressions to instantiate against.
    @param event_env The environment of events to instantiate against.
    @param program The program to instantiate.
    @return
      A result containing the instantiated program, updated event environment
      and updated expression environment, or a list of errors. *)
