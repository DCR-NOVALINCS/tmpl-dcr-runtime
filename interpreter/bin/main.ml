open Templating
open Api
open Errors
open Instantiation
open Typechecking
open Program_helper
open Cmds
open Misc
open Monads.ResultMonad
open Printing
open Input

(* =============================================================================
   Aux functions
   ============================================================================= *)

let rec start_header =
  CPrinter.cprint "To get started, type " ;
  CPrinter.cprint ~color:Green "help" ;
  (* CPrinter.cprint " or " ;
     CPrinter.cprint ~color:Green "h" ; *)
  CPrinter.cprintln " to see the available commands.\n"

and get_file_extension filename =
  let parts = String.split_on_char '.' filename in
  List.nth parts (List.length parts - 1)

and file_extension = "tdcr"

and input_file filename =
  if not (Sys.file_exists filename) then file_not_exists filename
  else
    get_file_extension filename
    |> function
    | ext when ext = file_extension -> return filename
    | got -> invalid_file_extension ~supported:file_extension ~got ()

let print_output ?(previous_state = empty_runtime_state) = function
  | Ok runtime_state ->
      let {output; _} = runtime_state in
      CPrinter.cprintln output ; return runtime_state
  | Error errors -> print_errors errors ; return previous_state

let set_logger logger_level =
  Logger.enable () ;
  match String.trim logger_level with
  | "debug" -> return @@ Logger.set_logger_level Debug
  | "info" -> return @@ Logger.set_logger_level Info
  | "warn" -> return @@ Logger.set_logger_level Warn
  | "error" -> return @@ Logger.set_logger_level Error
  | "success" -> return @@ Logger.set_logger_level Success
  | "" -> return @@ Logger.disable ()
  | _ -> invalid_logger_level logger_level

(* =============================================================================
   REPL Section
   ============================================================================= *)

open Cmdliner

type options = {logger_level: string}

(* let default_options = {logger_level= "debug"} *)

let rec interpret_command tokens runtime_state =
  match tokens with
  | [] -> return runtime_state
  | cmd_name :: _ -> (
    match List.assoc_opt cmd_name cmds with
    | None ->
        let open Misc.Bktree in
        let nearest, distance =
          nearest_neighbor levenshtein_distance cmds_bbk_tree cmd_name
        in
        invalid_command ~nearest ~distance tokens
    | Some {callback; _} -> (
        let argv = Array.of_list tokens in
        match Cmd.eval_value ~argv callback with
        | Ok (`Ok result) -> result runtime_state >>= fun state -> return state
        | _ -> return runtime_state ) )

and prompt runtime_state =
  start_header ;
  CPrinter.cprint ~color:BrightGreen "> " ;
  flush stdout ;
  (* Get command from input *)
  match input_line stdin with
  | exception End_of_file -> return runtime_state
  | input ->
      sanitize_input input
      >>= fun tokens ->
      interpret_command tokens runtime_state
      |> print_output ~previous_state:runtime_state
      >>= fun state -> prompt {state with output= ""}

let runtime options filename =
  set_logger options.logger_level
  >>= fun _ ->
  input_file filename
  >>= fun _ ->
  parse_program_from_file filename
  >>= fun program ->
  Logger.success "Parsed successfully" ;
  preprocess_program program
  >>= fun (event_env, expr_env, program) ->
  Logger.success "Preprocessed successfully" ;
  typecheck ~event_env program
  >>= fun (ty_env, event_env) ->
  Logger.success "Typechecked successfully" ;
  instantiate ~expr_env ~event_env program
  >>= fun (program, event_env, expr_env) ->
  Logger.success "Instantiated successfully" ;
  let runtime_state = mk_runtime_state ~ty_env ~expr_env ~event_env program in
  prompt runtime_state

let runtime_cmd =
  let info =
    let doc =
      "An implementation of a interpreter for Templates in DCR Graphs"
    in
    Cmd.info ~doc ~version:"0.1" "tmpl_dcr"
  and options =
    let logger_level =
      let doc = "The level of logging to be used" in
      Arg.(value & opt string "debug" & info ["l"; "logger-level"] ~doc)
    in
    Term.(const (fun logger_level -> {logger_level}) $ logger_level)
  and input_filename =
    let doc = "The input file to be processed" in
    Arg.(required & pos 0 (some string) None & info [] ~doc)
  in
  let input_program_main = Term.(const runtime $ options $ input_filename) in
  Cmd.v info input_program_main

let _ =
  (* Logger Settings *)
  Logger.enable () ;
  Logger.set_logger_level Debug ;
  match Cmd.eval_value runtime_cmd with
  | Ok (`Ok result) -> result |> print_output |> ignore
  | Error _ -> ()
  | _ -> ()
