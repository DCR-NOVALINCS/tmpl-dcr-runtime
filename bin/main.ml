open Templating.Syntax
open Templating.Runtime
open Templating.Battery_tests
open Templating.Instantiation
open Misc.Monads
(* open Misc.Env *)

(*
=============================================================================
  Main Section
=============================================================================
*)  

let execute_event ~event_id ?(expr = Unit)  program = 
  preprocess_program program
  >>= fun (event_env, expr_env) ->
  instantiate ~expr_env program
  >>= fun (program, expr_env) ->
  execute ~event_env ~expr_env ~event_id ~expr program
    
let view_program program = 
  preprocess_program program
  >>= fun (event_env, expr_env) ->
  view ~should_print_relations:true ~event_env ~expr_env program

(* TODO: *)
let parse_expression _expr_string = 
  Ok (IntLit (-1))

let read_command cmd program = 
  match cmd with
  | ["exit"] | ["quit"] | ["q"] -> exit 0
  | ["exec"; event_id; expr] | ["execute"; event_id; expr] | ["e"; event_id; expr] -> 
    parse_expression expr
    >>= fun parsed_expr ->
    execute_event ~event_id ~expr:parsed_expr program 
  | ["exec"; event_id] | ["execute"; event_id] | ["e"; event_id] -> 
    execute_event ~event_id program 
  | ["view"] | ["v"] -> 
    view_program program 
    >>= fun _ -> Ok program
  | _ -> print_endline "Invalid command"; Ok program

let rec prompt lexbuf program =
  print_string "> ";
  let cmd = read_line () |> String.split_on_char ' ' in
  read_command cmd program
  >>= fun program ->
  prompt lexbuf program

let _ = 
  let lexbuf = Lexing.from_channel stdin in
  let program = _test0 in
  prompt lexbuf program