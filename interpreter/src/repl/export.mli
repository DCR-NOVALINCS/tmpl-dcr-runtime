open Cmdliner
open State
open Ast
open Error

val term : (runtime_state -> (runtime_state, detailed_error list) result) Term.t
