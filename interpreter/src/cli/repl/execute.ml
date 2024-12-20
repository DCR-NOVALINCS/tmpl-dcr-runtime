open State
open Core
open Api
open Unparser
open Common
open Monads.ResultMonad
open Printing
open Cmdliner

let event_id =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"EVENT_ID" ~doc:"The event id to execute" )

and expr =
  Arg.(
    value & pos_right 0 string []
    & info [] ~docv:"EXPR_STRING" ~doc:"The expression to execute" )

and execute_cmd event_id expr_str state =
  let {ty_env; expr_env; event_env; program; _} = state in
  Logger.debug
  @@ Printf.sprintf "Executing event %s with expression %s" event_id
       (String.concat " " expr_str) ;
  parse_expression_from_string expr_str
  >>= fun expr ->
  execute ~ty_env ~expr_env ~event_env ~event_id ~expr:expr.data program
  >>= fun (program, event_env, expr_env) ->
  return
    { state with
      program
    ; event_env
    ; expr_env
    ; output=
        Printf.sprintf "Executed event %s with expression %s\n"
          (keyword event_id)
          (keyword (Plain.unparse_expr expr)) }

let term = Term.(const execute_cmd $ event_id $ expr)
