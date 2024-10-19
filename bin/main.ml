open Templating.Syntax
open Templating.Runtime
open Templating.Instantiation
open Templating.Lex_and_parse
open Misc.Monads
open Misc.Printing

(* open Misc.Env *)

(*
=============================================================================
  Aux functions
=============================================================================
*)  

let get_line_content filepath line =
  if Sys.file_exists filepath then
    let file = open_in filepath in
    let rec read_line n =
      match input_line file with
      | content when n = line -> content
      | _ -> read_line (n + 1)
    in
    let line_content = read_line 1 in
    close_in file;
    line_content
  else ""
  

let extract_location_info loc =
  match loc with 
  | Nowhere -> (0, 0, 0)
  | Location (start_pos, end_pos) -> 
    let line = start_pos.Lexing.pos_lnum in
    let start_char = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
    let end_char = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol in
    (line, start_char, end_char) 

(*┌*)
let print_error detailed_error =
  let { location ; message ; filepath } = detailed_error in
  let message_header = 
    CPrinter.eprint "error: "; 
    CPrinter.cprint (message ^ " ");
    CPrinter.cprint ~color:Cyan (string_of_loc location);
  
  in
  
  let message_file_section =
    begin match filepath with 
    | "" -> ()
    | _ -> 
      let line, start_char, end_char = extract_location_info location in
      let line_content = get_line_content filepath line in
      let marker =
        String.concat ""
          [ String.make start_char ' '
          ; CString.colorize ~color:Red
              (String.make (end_char - start_char) '^') ]
      in
      CPrinter.cprintf "  ──▶ %s:%d:%d\n" filepath line start_char ;
      CPrinter.cprintln "  │" ;
      CPrinter.cprintf "%d │ %s\n" line line_content ;
      CPrinter.cprintf "  │ %s\n" marker
    end in
  
  message_header;
  message_file_section

let print_output =
  function
  | Ok (_, msg) -> 
    CPrinter.cprintln msg;
    CPrinter.cprintln ""
  | Error e ->
    print_error e;
    CPrinter.cprintln ""

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

let help_message cmds = 
  let header = CString.colorize ~color:BrightCyan "Available Commands:" in
  let cmds_section = 
    cmds
    |> List.map (fun (cmd, alias, desc) -> 
      Printf.sprintf "- %s (%s): %s" (CString.colorize ~color:Green cmd) (CString.colorize ~color:Green alias) desc)
    |> String.concat "\n" in
  String.concat "\n" [header; cmds_section]

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
let parse_expression expr_string = 
  let expr = expr_string |> String.concat "" in
  let expr_lexbuf = Lexing.from_string (expr) in
  Logger.info "Parsing expression";
  Logger.debug @@ "Expression: " ^ expr;
  parse_expression expr_lexbuf
  (* let n = Random.full_int 10 in
  Ok (annotate ~loc:Nowhere (IntLit n)) *)

let read_command cmd program = 
  Logger.debug @@ "Command: " ^ (String.concat " | " cmd);
  match cmd with
  | ["exit"] | ["q"] -> exit 0

  | ["help"] | ["h"] -> 
    Ok (program, help_message [
      ("view", "v", "View the current program")
      ; ("debug", "d", "View the current program with relations")
      ; ("exec", "e", "Execute an event with an expression")
      ; ("exit", "q", "Exit the program")
      ; ("help", "h", "Display this message")
    ])
    (* Ok (program, 
    CString.colorize ~color:BrightCyan "Available Commands:\n" ^
    "- view (v): View the current program\n\
    - debug (d): View the current program with relations\n\
    - exec (e) <event_id> <expr>: Execute an event with an expression\n\
    - exit (q): Exit the program\n\
    - help (h): Display this message") *)

  | "exec"::event_id::expr | "e"::event_id::expr -> 
    (if expr = [] then Ok (annotate Unit) else
    parse_expression expr)
    >>= fun parsed_expr ->
    execute_event ~event_id ~expr:parsed_expr.data program
    >>= fun program -> 
    Ok (program, 
    "Event executed with expression " ^ (CString.colorize ~color:Yellow @@ string_of_expr parsed_expr))

  | ["view"] | ["v"] -> 
    view_program program 
    >>= fun unparsed_program -> Ok (program, unparsed_program)

  | ["debug"] | ["d"] -> 
    debug_program program 
    >>= fun unparsed_program -> Ok (program, unparsed_program)

  | _ -> invalid_command cmd

let sanatize_input input = 
  input
  |> String.split_on_char ' ' 
  |> List.filter (fun s -> s <> "")
  |> Result.ok

let rec prompt program =
  CPrinter.cprint ~color:BrightGreen "> " ;

  (* Read line *)
  sanatize_input (read_line ())
  >>= fun cmd ->

  (* Process command *)
  read_command cmd program
  |> print_output; 
     prompt program
    
let parse filename = 
  Logger.debug @@ "Reading file: ";
  let lexbuf = Lexing.from_channel (open_in filename) in
  let prog = parse_program lexbuf in
  prog

let runtime = 
  (* Logger settings *)
  Logger.enable () ;
  Logger.set_logger_level Debug;
  (* --- Main program --- *)
  (* Get & Parse the initial input *)
  ( let filename = Sys.argv.(1) in
  parse filename
  >>= fun program ->
  
  (* Preprocess program *)
  Logger.info "Program parsed successfully";
  preprocess_program program
  >>= fun (_, expr_env) ->
  
  (* Instantiate initial templates *)
  instantiate ~expr_env program
  >>= fun (program, _) ->
    
  (* Display welcome message *)
  CPrinter.cprint "To get started, type ";
  CPrinter.cprint ~color:Green "help";
  CPrinter.cprint " or ";
  CPrinter.cprint ~color:Green "h";
  CPrinter.cprintln " to see the available commands.\n";

  (* Start the prompt *)
  prompt program
  ) |> print_output

let _ = runtime