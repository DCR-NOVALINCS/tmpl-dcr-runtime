open Templating.Syntax
open Templating.Api
open Templating.Instantiation
open Templating.Lex_and_parse
open Templating.Typechecking
open Templating.Errors
open Misc.Monads
open Misc.Printing

(* open Misc.Env *)

(*
=============================================================================
  Aux functions
=============================================================================
*)  

let print_output ?(previous_program = empty_program) =
  function  
  | Ok (program, msg) -> 
    CPrinter.cprintln msg;
    Ok program
  | Error errors -> 
    List.iter print_error errors;
    Ok (previous_program)


let sanatize_input input = 
  input
  |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> Result.ok

(* let sanatize_inputs inputs = 
  inputs
  |> List.map sanatize_input
  |> Result.ok *)

let help_message cmds = 
  let header = CString.colorize ~color:BrightCyan "Available Commands:" in
  let cmds_section = 
    cmds
    |> List.map (fun { name; alias; params; desc } -> 
      Printf.sprintf "- %s (%s) %s : %s" (CString.colorize ~color:Green name) (CString.colorize ~color:Green alias) (String.concat " " (List.map (CString.colorize ~color:Red) params)) desc)
    |> String.concat "\n" in
  String.concat "\n" [header; cmds_section]

(* TODO: *)
let parse_expression expr_string = 
  let expr = expr_string |> String.concat " " in
  let expr_lexbuf = Lexing.from_string (expr) in
  Logger.debug @@ "Parsing expression " ^ expr;
  parse_expression expr_lexbuf

(*
=============================================================================
  Main Section
=============================================================================
*)  

let read_command tokens program = 
  Logger.debug @@ "Command: " ^ (String.concat " | " tokens);
  match tokens with
  | ["exit"] | ["q"] -> exit 0

  | ["help"] | ["h"] -> 
    Ok (program, help_message cmds)

  | "parse"::filename::[] | "p"::filename::[] -> 
    let parse_file filename = 
      let lexbuf = Lexing.from_channel (open_in filename) in
      parse_program ~filename lexbuf
      >>= fun program -> 
      Ok (program, "File " ^ (CString.colorize ~color:Yellow filename) ^ " parsed successfully")
    in
    parse_file filename

  | "exec"::event_id::expr | "e"::event_id::expr -> 
    (if expr = [] 
      then Ok (annotate Unit) 
    else parse_expression expr)
    >>= fun parsed_expr ->
    typecheck_expr parsed_expr
    >>= fun _ ->
    execute ~event_id ~expr:parsed_expr.data program
    >>= fun program -> 
    Ok (program, 
    "Event executed with expression " ^ (CString.colorize ~color:Yellow @@ string_of_expr parsed_expr))

  | ["view"] | ["v"] -> 
    view program 
    >>= fun unparsed_program -> Ok (program, unparsed_program)

  | ["debug"] | ["d"] -> 
    view_debug program 
    >>= fun unparsed_program -> Ok (program, unparsed_program)

  | "export"::filenames | "exp"::filenames -> 
    (* FIXME: add specific function to do this *)
    (* view_debug program  *)
    unparse_program program
    >>= fun unparsed_program ->
    let write_to_file filename = 
      let oc = open_out filename in
      Printf.fprintf oc "%s\n" unparsed_program;
      close_out oc 
    in
    List.iter write_to_file filenames;
    Ok (program, "Program exported to " ^ CString.colorize ~color:Yellow (String.concat ", " filenames))

  | [] -> Ok (program, "")

  | _ -> 
    let open Misc.Bktree in
    let line = String.concat " " tokens in
    let nearest, distance = nearest_neighbor levenshtein_distance cmds_bbk_tree line in
    invalid_command ~nearest ~distance tokens

let rec prompt program =
  CPrinter.cprint ~color:BrightGreen "> " ;

  (* Read line *)
  sanatize_input (read_line ())
  >>= fun tokens ->

  (* Process command *)
  read_command tokens program
  |> print_output ~previous_program:program
  >>= fun program ->

  (* Continue... *)
  prompt program
    
let parse filename = 
  Logger.debug @@ "Reading file: " ^ (CString.colorize ~color:Yellow filename);
  let lexbuf = Lexing.from_channel (open_in filename) in
  let prog = parse_program ~filename lexbuf in
  prog

let runtime = 
  (* Logger settings *)
  (* Logger.disable () ; *)
  Logger.set_logger_level Debug;
  (* --- Main program --- *)
  (* Get & Parse the initial input *)
  (
  let start_timer = Sys.time () in
  let filename = Sys.argv.(1) in
  
  (* FIXME: Better variable name! *)
  let entry = if not @@ Sys.file_exists filename then Ok (empty_program)
  else parse filename in 
  
  entry
  >>= fun program ->

  (* Preprocess program *)
  Logger.info "Program parsed successfully";
  preprocess_program program
  >>= fun (_, expr_env, program) ->
  
  (* Typecheck program *)
  (* typecheck program 
  >>= fun _ -> *)

  (* Instantiate initial templates *)
  instantiate ~expr_env program
  >>= fun (program, _) ->
    
  (* Display welcome message *)
  let loaded_header =
    let elapsed_time = (Sys.time () -. start_timer) *. 1_000. in (* in ms *)
    CPrinter.cprint "Program loaded in ";
    CPrinter.cprintf ~color:Yellow "%.2fms" elapsed_time;
    CPrinter.cprintln "."
  in
  let start_header = 
    CPrinter.cprint "To get started, type ";
    CPrinter.cprint ~color:Green "help";
    CPrinter.cprint " or ";
    CPrinter.cprint ~color:Green "h";
    CPrinter.cprintln " to see the available commands.\n";
  in
  loaded_header;
  start_header;
  (* Start the prompt *)
  prompt program)
  |> print_output
  

let _ = runtime
