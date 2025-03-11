open State
open Unparser
open Common
open Env
open Monads.ResultMonad
open Printing
open Cmdliner

(** [view_envs] flag to view the current environments
    @return a boolean flag *)
let view_envs =
  Arg.(
    value & flag
    & info ["e"; "envs"] ~docv:"ENVS" ~doc:"View the current environments" )

(** [debug_cmd] command to view debug information.
    @param view_envs a boolean flag to view the current environments
    @param state the current state
    @return the new state displaying debug information. *)
and debug_cmd view_envs state =
  let {ty_env; event_env; expr_env; _} = state in
  let buffer = Buffer.create 100 in
  Buffer.add_string buffer "\n" ;
  let _ =
    match view_envs with
    | false -> ()
    | true ->
        Buffer.add_string buffer
          (CString.colorize ~color:Magenta ~format:Bold "Type Environment:\n") ;
        Buffer.add_string buffer (string_of_env Plain.unparse_ty ty_env) ;
        Buffer.add_string buffer
          (CString.colorize ~color:Magenta ~format:Bold
             "\n\nEvent Environment:\n" ) ;
        Buffer.add_string buffer
          (string_of_env (fun e -> Plain.unparse_events [e]) event_env) ;
        Buffer.add_string buffer
          (CString.colorize ~color:Magenta ~format:Bold
             "\n\nExpression Environment:\n" ) ;
        Buffer.add_string buffer
          (string_of_env (fun e -> Plain.unparse_expr e) expr_env)
  in
  let output = Buffer.contents buffer in
  return {state with output}

let term = Term.(const debug_cmd $ view_envs)
