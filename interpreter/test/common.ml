open OUnit2
(* open Misc.Env *)
open Misc.Monads
open Misc.Printing
open Templating.Instantiation
open Templating.Api
open Templating.Errors
open Templating.Syntax
open Templating.Lex_and_parse
(* open Utils *)

let root_path = "../../../"

let parse filename = 
  try 
    let filepath = Printf.sprintf "%s%s" root_path filename in
    let file = open_in filepath in 
    let lexbuf = Lexing.from_channel file in
    parse_program ~filename lexbuf
    >>| fun program ->
    close_in file; 
    program
  with 
  | Sys_error msg -> Error [{
    location = Nowhere;
    message = Printf.sprintf "File not found: %s. Current directory %s" filename (Sys.getcwd ());
    hint = Some msg
  }]
  | _ -> should_not_happen "Unknown error"

let setup filename _test_ctxt =
  Logger.disable ();
  parse filename 
  >>= fun program -> 
  preprocess_program program 
  >>= fun (event_env, expr_env, program) ->
  instantiate ~expr_env program
  >>= fun (program, expr_env) ->
  Ok (program, event_env, expr_env)

let teardown _state _test_ctxt = ()

let build_state filename test_ctxt = bracket (setup filename) teardown test_ctxt

let print_error e = 
  let string_location = 
    function
    | Nowhere -> "Unknown location"
    | Location (start_pos, _end_pos, filename) -> 
      Printf.sprintf "%s:%d:%d" (Option.value ~default:"" filename) start_pos.pos_lnum start_pos.pos_cnum in
  let string_hint = 
    function
    | Some hint -> Printf.sprintf "Hint: %s" hint
    | None -> "" in
  Printf.sprintf "%s\n%s\n%s" (string_location e.location) e.message (string_hint e.hint)

let process_result = 
  function
  | Ok _ -> ()
  | Error e -> 
    List.map (fun e -> print_error e) e
    |> String.concat "\n"
    |> assert_failure