open Syntax
open Misc.Env

val typecheck : program -> (unit, detailed_error list) result

val typecheck_expr : expr -> expr env -> (unit, detailed_error list) result
