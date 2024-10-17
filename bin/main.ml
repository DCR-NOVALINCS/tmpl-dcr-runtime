open Templating.Syntax
open Templating.Runtime
open Templating.Battery_tests
open Templating.Instantiation
open Misc.Monads
open Misc.Printing

(* open Misc.Env *)

(*
=============================================================================
  Aux functions
=============================================================================
*)  

(* let get_line_content file line =
  let ic = open_in file in
  let rec read_line n =
    match input_line ic with
    | content when n = line ->
        content
    | _ ->
        read_line (n + 1)
  in
  let line = read_line 1 in
  close_in ic ; line *)

(* let extract_location_info loc =
  match loc with 
  | Nowhere -> (0, 0, 0)
  | Location (start_pos, end_pos) -> 
    let line = start_pos.Lexing.pos_lnum in
    let start_char = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
    let end_char = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol in
    (line, start_char, end_char)  *)

(*┌*)
let print_error detailed_error =
  let { location = _ ; message ; filepath = _ } = detailed_error in
  (* let line, start_char, end_char = extract_location_info location in *)
  (* let line_content = get_line_content file line in *)
  (* let line_content = "" in *)
  CPrinter.eprintf "error: %s\n" message ; ()
  (* CPrinter.cprintf "  ──▶ %s:%d:%d\n" filepath line start_char ;
  CPrinter.cprintln "  │" ;
  CPrinter.cprintf "%d │ %s\n" line line_content ;
  let marker =
    String.concat ""
      [ String.make start_char ' '
      ; CString.colorize ~color:ASNIIColor.Red
          (String.make (end_char - start_char) '^') ]
  in
  CPrinter.cprintf "  │ %s\n" marker *)

(*
=============================================================================
  Error messages
=============================================================================
*)  

let invalid_command cmd = 
  Error {
    location = Nowhere
    ; message = Printf.sprintf "Invalid command %s" (String.concat " " cmd)
    ; filepath = ""
  }

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
  Ok (annotate ~loc:Nowhere Unit)

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
    execute_event ~event_id ~expr:parsed_expr.data program
    >>= fun program -> Ok (program, "Event executed with expression " ^ (string_of_expr parsed_expr))
  | ["view"] | ["v"] -> 
    view_program program 
    >>= fun unparsed_program -> Ok (program, unparsed_program)
  | ["debug"] | ["d"] -> 
    debug_program program 
    >>= fun unparsed_program -> Ok (program, unparsed_program)
  | _ -> invalid_command cmd

let rec prompt lexbuf program =
  CPrinter.cprint ~color:BrightGreen "> " ;
  let cmd = read_line () 
  |> String.split_on_char ' ' 
  |> List.filter (fun s -> s <> "") in
  read_command cmd program
  |> function
  | Ok (program, msg) -> 
    CPrinter.cprintln msg;
    CPrinter.cprintln "";
    prompt lexbuf program
  | Error e ->
    print_error e;
    prompt lexbuf program

let _ = 
  Logger.enable () ;
  (* Logger.set_logger_level Error; *)
  let lexbuf = Lexing.from_channel stdin in
  let program = _test0 in
  preprocess_program program
  >>= fun (_, expr_env) ->
  instantiate ~expr_env program
  >>= fun (program, _) ->
  CPrinter.cprint "To get started, type ";
  CPrinter.cprint ~color:Green "help";
  CPrinter.cprintln " to see the available commands.\n";
  prompt lexbuf program