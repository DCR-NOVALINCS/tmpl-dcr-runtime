open Common
open Env
open Printing
open Monads.ResultMonad
open Ast
open Syntax
open Error
open Typing
open Helper
open Cmdliner

(* =============================================================================
   Runtime State Management Section
   ============================================================================= *)

type runtime_state =
  { ty_env: type_expr' env
  ; label_types: event_type_value Hashtbl.Make(String).t
  ; expr_env: expr env
  ; event_env: event env
  ; program: program
  ; previous_state: runtime_state option
  ; output: string }

let mk_runtime_state ?(output = "") ?(ty_env = empty_env)
    ?(label_types = EventTypes.empty) ?(expr_env = empty_env)
    ?(event_env = empty_env) program =
  { ty_env
  ; label_types
  ; expr_env
  ; event_env
  ; program
  ; previous_state= None
  ; output }

let empty_runtime_state = mk_runtime_state empty_program

let string_of_state state =
  let {output; _} = state in
  output

let print_output ?(previous_state = empty_runtime_state) = function
  | Ok runtime_state ->
      CPrinter.cprintln (string_of_state runtime_state) ;
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
  let cmd =
    { params
    ; description
    ; callback=
        Cmd.v (Cmd.info ~man:[`S "DESCRIPTION"; `P description] name) term }
  in
  (name, cmd) :: cmds
