open State
open Ast
open Error

val runtime : string -> (runtime_state, detailed_error list) result
