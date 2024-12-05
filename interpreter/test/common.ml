open OUnit2
open Misc
open Monads.ResultMonad
open Templating
open Errors
open Syntax
open Lex_and_parse
open Program_helper

let root_path = "../../../"

let parse filename =
  try
    let filepath = Printf.sprintf "%s%s" root_path filename in
    let file = open_in filepath in
    let lexbuf = Lexing.from_channel file in
    parse_program ~filename lexbuf >>| fun program -> close_in file ; program
  with
  | Sys_error _msg -> file_not_exists filename
  | _ -> should_not_happen "Unknown error"

let setup filename _test_ctxt =
  (* Logger.disable () ; *)
  parse filename
  >>= fun program ->
  preprocess_program program
  >>= fun (event_env, expr_env, program) ->
  (* instantiate ~expr_env program *)
  (* >>= fun (program, expr_env) ->  *)
  return (program, event_env, expr_env)

let teardown _state _test_ctxt = ()

let build_state filename test_ctxt = bracket (setup filename) teardown test_ctxt

let expecting_ok = function
  | Error e -> pretty_string_errors e |> assert_failure
  | _ as result -> result

let expecting_error ?(msg = "") = function
  | Ok _ -> assert_failure @@ Printf.sprintf "Expecting error: %s" msg
  | _ as result -> result
