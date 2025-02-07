open Helper
open Ast
open Syntax
open Error
open Common
open Env

(* type event_kind = type_expr' * event_type'

type event_type_value = Undefined | Defined of event_kind *)

val typecheck :
     ?event_env:event' annotated env
  -> program
  -> (type_expr' env * event' annotated env, detailed_error list) result
(** [typecheck ?event_env program] typechecks the [program] by ensuring that:
    - All event expressions are well-typed according to the [event_env].
    - Template instance expressions are well-typed according to the [event_env]
      and [expr_env].
    - All relations find their corresponding events in the [event_env].

    @param event_env The environment of events to typecheck against.
    @param program The program to typecheck.
    @return
      A result containing the type environment and event environment if the
      program is well-typed, or a list of errors if the program is not
      well-typed. *)

val typecheck_expr :
     ?ty_env:type_expr' env
  -> ?label_types:event_type_value Hashtbl.Make(String).t
  -> expr
  -> (type_expr', detailed_error list) result
(** [typecheck_expr ?ty_env ?label_types expr] typechecks the [expr] by ensuring
    that:
    - All expressions are well-typed according to the [ty_env].
    - All event expressions are well-typed according to the [label_types].

    @param ty_env The environment of types to typecheck against.
    @param label_types The environment of event types to typecheck against.
    @param expr The expression to typecheck.
    @return
      A result containing the type of the expression if the expression is
      well-typed, or a list of errors if the expression is not well-typed. *)

val equal_types : type_expr' -> type_expr' -> bool
(** {i (tail recursive)} [equal_types type_1 type_2] indicates whether type
    expressions [type_1] and [type_2] are structurally equal .

    Returns {b true} if the [type_1] and [type_2] are structurally equal, and
    {b false} otherwise. *)
