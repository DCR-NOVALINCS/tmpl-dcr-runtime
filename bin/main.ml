open Templating.Syntax
open Templating.Api
open Templating.Instantiation
open Templating.Lex_and_parse
open Misc.Monads
open Misc.Printing
open Templating.Errors

(* open Misc.Env *)



(*
=============================================================================
  Aux functions
=============================================================================
*)  

let print_output =
  function
  | Ok (_, msg) -> 
    CPrinter.cprintln msg
    (* CPrinter.cprintln "" *)
  | Error errors ->
    List.iter print_error errors;
    CPrinter.cprintln ""

let sanatize_input input = 
  input
  |> String.split_on_char ' ' 
  |> List.filter (fun s -> s <> "")
  |> Result.ok

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

let read_command tokens program = 
  Logger.debug @@ "Command: " ^ (String.concat " | " tokens);
  match tokens with
  | ["exit"] | ["q"] -> exit 0

  | ["help"] | ["h"] -> 
    Ok (program, help_message [
      ("view", "v", "View the current program")
      ; ("debug", "d", "View the current program with relations")
      ; ("exec", "e", "Execute an event with an expression")
      ; ("exit", "q", "Exit the program")
      ; ("help", "h", "Display this message")
    ])

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

  | [] -> Ok (program, "")

  | _ -> invalid_command tokens

let rec prompt program =
  CPrinter.cprint ~color:BrightGreen "> " ;

  (* Read line *)
  sanatize_input (read_line ())
  >>= fun tokens ->

  (* Process command *)
  read_command tokens program
  |> print_output; 
     prompt program
    
let parse filename = 
  Logger.debug @@ "Reading file: " ^ (CString.colorize ~color:Yellow filename);
  let lexbuf = Lexing.from_channel (open_in filename) in
  let prog = parse_program ~filename lexbuf in
  prog

let runtime = 
  (* Logger settings *)
  (* Logger.enable () ; *)
  Logger.disable () ;
  Logger.set_logger_level Debug;
  (* --- Main program --- *)
  (* Get & Parse the initial input *)
  ( let filename = Sys.argv.(1) in
  
  (* FIXME: Better variable name! *)
  let entry = if not @@ Sys.file_exists filename then Ok (empty_program)
  else parse filename in 
  
  entry
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