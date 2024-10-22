open Js_of_ocaml
open Misc.Monads
open Misc.Printing
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

(* TODO: Maybe put this in another place to preserve the state... *)
let active_program = ref empty_program

let stringify_errors errors = 
  List.map (fun e -> e.message |> CString.colorize ~color:Red) errors
  |> String.concat "\n"
  |> Js.string

let view =
  view_enabled !active_program
  |> function
  | Error errors -> 
    stringify_errors errors
  | Ok unparsed_program -> Js.string unparsed_program

let debug_view =
  view_debug !active_program
  |> function
  | Error errors -> 
    stringify_errors errors
  | Ok unparsed_program -> Js.string unparsed_program

let execute event_id expr = 
  ( let expr_lexbuf = Lexing.from_string expr in
  parse_expression expr_lexbuf
  >>= fun expr ->
  execute ~event_id ~expr:expr.data !active_program )
  |> function
  | Error errors -> 
    stringify_errors errors
  | Ok program -> 
    active_program := program; 
    Js.string @@ "Executed successfully with event id " ^ event_id

let parse program_str = 
  let program_lexbuf = Lexing.from_string program_str in
  parse_program program_lexbuf
  |> function
  | Error errors -> 
    stringify_errors errors
  | Ok program -> 
    active_program := program; 
    Js.string @@ CString.colorize ~color:Green ("Parsed successfully\n" ^ string_of_program program)

let runtime = 
  Js.export "view" (fun () -> view);
  Js.export "debugView" (fun () -> debug_view);
  Js.export "execute" (fun event_id expr -> execute (Js.to_string event_id) (Js.to_string expr));
  Js.export "parse" (fun program_str -> parse (Js.to_string program_str));
  ()

let _ = runtime
