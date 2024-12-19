open Common
open Env
open Printing
open Monads.ResultMonad
open Ast
open Syntax
open Error
open Cmdliner

(* =============================================================================
   Runtime State Management Section
   ============================================================================= *)

type runtime_state =
  { ty_env: type_expr' env
  ; expr_env: expr env
  ; event_env: event env
  ; program: program
  ; output: string }

let mk_runtime_state ?(output = "") ?(ty_env = empty_env)
    ?(expr_env = empty_env) ?(event_env = empty_env) program =
  {ty_env; expr_env; event_env; program; output}

let empty_runtime_state = mk_runtime_state empty_program

let string_of_state state =
  let {output; _} = state in
  CString.colorize ~format:Bold output

let print_output ?(previous_state = empty_runtime_state) = function
  | Ok runtime_state ->
      CPrinter.cprintln ~format:Bold (string_of_state runtime_state) ;
      return runtime_state
  | Error errors -> print_errors errors ; return previous_state

(* =============================================================================
   Command Creation Section
   ============================================================================= *)

type cmd =
  { params: string list
  ; description: string
  ; callback:
      (runtime_state -> (runtime_state, detailed_error list) result) Cmd.t }

let create_cmd (name, params, description) term cmds =
  (* Add both the default and shortened versions of the command to the list *)
  let cmd =
    {params; description; callback= Cmd.v (Cmd.info ~man:[] name) term}
  in
  (name, cmd) :: cmds
