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

let execute_event ~event_id ?(expr = Unit) program = 
  preprocess_program program
  >>= fun (event_env, expr_env) ->
  instantiate ~expr_env program
  >>= fun (program, expr_env) ->
  execute ~event_env ~expr_env ~event_id ~expr program
    
let view_program program = 
  preprocess_program program
  >>= fun (event_env, expr_env) ->
  view_enabled ~event_env ~expr_env program

let debug_program program = 
  preprocess_program program
  >>= fun (event_env, expr_env) ->
  view ~should_print_relations:true ~event_env ~expr_env program

(* TODO: *)
let parse_expression _expr_string = 
  (* let n = Random.full_int 2 in *)
  Ok (IntLit 5)

let read_command cmd program = 
  match cmd with
  | ["exit"] | ["q"] -> exit 0
  | ["help"] | ["h"] -> 
    Ok (program, 
    "Commands: \n\
    - view (v): View the current program\n\
    - debug (d): View the current program with relations\n\
    - exec (e) <event_id> <expr>: Execute an event with an expression\n\
    - exit (q): Exit the program\n\
    - help (h): Display this message")
  | "exec"::event_id::expr | "e"::event_id::expr -> 
    parse_expression expr
    >>= fun parsed_expr ->
    execute_event ~event_id ~expr:parsed_expr program
    >>= fun program -> Ok (program, "Event executed with expression " ^ (string_of_expr parsed_expr))
  | ["view"] | ["v"] -> 
    view_program program 
    >>= fun unparsed_program -> Ok (program, unparsed_program)
  | ["debug"] | ["d"] -> 
    debug_program program 
    >>= fun unparsed_program -> Ok (program, unparsed_program)
  | _ -> Error "Invalid command"

let rec prompt lexbuf program =
  print_string "> ";
  let cmd = read_line () 
  |> String.split_on_char ' ' 
  |> List.filter (fun s -> s <> "") in
  read_command cmd program
  |> function
  | Ok (program, msg) -> 
    print_endline msg;
    prompt lexbuf program
  | Error e ->
    print_endline e;
    prompt lexbuf program

let _ = 
  let lexbuf = Lexing.from_channel stdin in
  let program = _test10 in
  preprocess_program program
  >>= fun (_, expr_env) ->
  instantiate ~expr_env program
  >>= fun (program, _) ->
  prompt lexbuf program