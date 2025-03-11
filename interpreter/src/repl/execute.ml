open State
open Core
open Api
open Unparser
open Common
open Monads.ResultMonad
open Printing
open Cmdliner

(** [event_id] the event id argument to execute
    @return a string option *)
let event_id =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"EVENT_ID" ~doc:"The event id to execute" )

(** [expr] the expression argument, as [string], to execute
    @return a string list *)
and expr =
  Arg.(
    value & pos_right 0 string []
    & info [] ~docv:"EXPR_STRING" ~doc:"The expression to execute" )

(** [execute_cmd] command to execute the event with the expression
    @param event_id the event id to execute
    @param expr_str the expression to execute
    @param state the current state
    @return the new state with the output *)
and execute_cmd event_id expr_str state =
  let {ty_env; label_types; expr_env; event_env; program; _} = state in
  Logger.debug
  @@ Printf.sprintf "Executing event %s with expression %s" event_id
       (String.concat " " expr_str) ;
  let* expr = parse_expression_from_string expr_str in
  let* program, event_env, expr_env, ty_env, label_types =
    execute ~ty_env ~label_types ~expr_env ~event_env ~event_id ~expr:expr.data
      program
  in
  return
    { program
    ; ty_env
    ; label_types
    ; event_env
    ; expr_env
    ; previous_state= Some state
    ; output=
        Printf.sprintf "Executed event %s with expression %s\n"
          (keyword event_id)
          (keyword (Plain.unparse_expr expr)) }

let term = Term.(const execute_cmd $ event_id $ expr)
