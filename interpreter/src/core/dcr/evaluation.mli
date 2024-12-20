open Ast
open Syntax
open Error
open Common
open Env

val eval_expr :
  expr' annotated -> expr' annotated env -> (expr, detailed_error list) result
(** [eval_expr expr expr_env] evaluates the given expression [expr] in the
    environment [expr_env].
    @param expr The expression to evaluate.
    @param expr_env The environment in which to evaluate the expression.
    @return The result of evaluating the expression. *)

val partial_eval_expr :
  expr' annotated -> expr' annotated env -> (expr, detailed_error list) result
(** [partial_eval_expr expr expr_env] partially evaluates the given expression
    [expr] in the environment [expr_env], i.e. evaluates the expression as much
    as possible without knowing the values of all the variables. This is useful
    for evaluating expressions in the context of a specific environment or for
    optimizing expressions.
    @param expr The expression to partially evaluate.
    @param expr_env
      The environment in which to partially evaluate the expression.
    @return The result of partially evaluating the expression. *)
