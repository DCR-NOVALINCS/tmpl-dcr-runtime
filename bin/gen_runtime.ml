open Js_of_ocaml
open Misc.Monads
(* open Misc.Printing *)
open Templating.Api
(* open Templating.Errors *)
open Templating.Syntax
open Templating.Lex_and_parse
(* open Misc.Monads *)

(*
==============================
  Runtime for the web editor

  Available functions:
  - view
  - view debug
  - execute
  - parse

*)

exception ExecutionError of detailed_error list

(* TODO: Maybe put this in another place to preserve the state... *)
let active_program = ref empty_program

(* let stringify_errors errors = 
  let module CString = String in
  List.map (fun e -> e.message |> CString.colorize ~color:Red) errors
  |> String.concat "\n" *)

let raise_errors errors = 
  raise (ExecutionError errors)

let print_output ~ok = 
  function
  | Ok _ as res -> 
    ok res
  | Error errors ->
    raise_errors errors

let view =
  view !active_program
  |> print_output 
    ~ok:(fun res -> Js.string @@ Result.get_ok res)

let debug_view =
  view_debug !active_program
  |> print_output
    ~ok:(fun res -> Js.string @@ Result.get_ok res)

let execute event_id expr = 
  ( let expr_lexbuf = Lexing.from_string expr in
  parse_expression expr_lexbuf
  >>= fun expr ->
  execute ~event_id ~expr:expr.data !active_program )
  |> print_output
    ~ok:(fun program_res -> 
      let program = Result.get_ok program_res in
      active_program := program;
      Js.string @@ "Executed successfully\n")

let parse program_str = 
  let program_lexbuf = Lexing.from_string program_str in
  parse_program program_lexbuf
  |> print_output
    ~ok:(fun program_res -> 
      let program = Result.get_ok program_res in
      active_program := program;
      Js.string @@ "Parsed successfully\n")

let runtime = 
  Js.export "view" (fun () -> view);
  Js.export "debugView" (fun () -> debug_view);
  Js.export "execute" (fun event_id expr -> execute (Js.to_string event_id) (Js.to_string expr));
  Js.export "parse" (fun program_str -> parse (Js.to_string program_str));
  ()

let _ = runtime
