open Templating.Syntax
open Templating.Runtime
open Templating.Battery_tests
open Templating.Instantiation
open Misc.Monads
open Misc.Printing

module CPrinter = MakePrinter (ASNIIColor)
module CString = ASNIIString (ASNIIColor)
module Logger = MakeLogger (ASNIIColor)

(* open Misc.Env *)

(*
=============================================================================
  Aux functions
=============================================================================
*)  

(* type detailed_error =
  {location: int; message: string; error_type: string}

let mk_detail_error ~filename ~loc ~msg ~error_type =
  {location= 0; message= msg; error_type}

let get_line_content file line =
  let ic = open_in file in
  let rec read_line n =
    match input_line ic with
    | content when n = line ->
        content
    | _ ->
        read_line (n + 1)
  in
  let line = read_line 1 in
  close_in ic ; line

let extract_location_info loc =
  let {filename; location= loc} = loc in
  match loc with
  | Nowhere ->
      (filename, 0, 0, 0)
  | Range (start_p, end_p) ->
      ( filename
      , start_p.pos_lnum
      , start_p.pos_cnum - start_p.pos_bol
      , end_p.pos_cnum - start_p.pos_bol )
  | Position pos ->
      ( filename
      , pos.pos_lnum
      , pos.pos_cnum - pos.pos_bol
      , pos.pos_cnum - pos.pos_bol )

(*┌*)
let print_error {location= loc; message= msg; error_type= _} =
  let open Misc in
  let open Printing in
  let file, line, start_char, end_char = extract_location_info loc in
  let line_content = get_line_content file line in
  CPrinter.eprintf "error: %s\n" msg ;
  CPrinter.cprintf "  ──▶ %s:%d:%d\n" file line start_char ;
  CPrinter.cprintln "  │" ;
  CPrinter.cprintf "%d │ %s\n" line line_content ;
  let marker =
    String.concat ""
      [ String.make start_char ' '
      ; ColorString.colorize ~color:ASNIIColor.Red
          (String.make (end_char - start_char) '^') ]
  in
  CPrinter.cprintf "  │ %s\n" marker *)

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
  Ok (Record [("id", IntLit 1); ("count", IntLit 2); ("typename", StringLit "tree")])

let read_command cmd program = 
  match cmd with
  | ["exit"] | ["q"] -> exit 0
  | ["help"] | ["h"] -> 
    Ok (program, 
    CString.colorize ~color:ASNIIColor.BrightCyan "Available Commands:\n" ^
    "- view (v): View the current program\n\
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
    CPrinter.eprint "error: ";
    CPrinter.eprintln e;
    prompt lexbuf program

let _ = 
  let lexbuf = Lexing.from_channel stdin in
  let program = _test4 in
  preprocess_program program
  >>= fun (_, expr_env) ->
  instantiate ~expr_env program
  >>= fun (program, _) ->
  prompt lexbuf program