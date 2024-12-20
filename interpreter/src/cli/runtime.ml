open Options
open Errors
open Common
open Monads.ResultMonad
open Printing
open Env
open Checkable
open Repl.Main
open Repl.State
open Core
open Api
open Typing.Typechecking
open Dcr
open Instantiation
open Helper
open Ast

(* open Error *)
open Unparser
open Cmdliner

let info =
  let doc = "An implementation of a interpreter for Templates in DCR Graphs" in
  Cmd.info ~doc ~version:"0.1" "tmpl_dcr"

let options =
  let logger_level =
    let doc = "The level of logging to be used" in
    Arg.(value & opt string "" & info ["l"; "log"] ~doc)
  in
  Term.(
    const (fun ll ->
        {logger_level= (if String.trim ll = "" then None else Some ll)} )
    $ logger_level )

let input_filename =
  let doc = "The input file to be processed" in
  Arg.(required & pos 0 (some string) None & info [] ~doc)

let get_file_extension filename =
  let parts = String.split_on_char '.' filename in
  List.nth parts (List.length parts - 1)

let file_extension = "tdcr"

let input_file filename =
  if not (Sys.file_exists filename) then file_not_exists filename
  else
    get_file_extension filename
    |> function
    | ext when ext = file_extension -> return filename
    | got -> invalid_file_extension ~supported:file_extension ~got ()

let interpret_command tokens runtime_state =
  match tokens with
  | [] -> return runtime_state
  | name :: _ -> (
    match List.assoc_opt name commands with
    | None ->
        let open Bktree in
        let nearest, distance =
          nearest_neighbor levenshtein_distance cmds_bbk_tree name
        in
        invalid_command ~nearest ~distance tokens
    | Some {callback; _} -> (
        let argv = Array.of_list tokens in
        match Cmd.eval_value ~argv callback with
        | Ok (`Ok cmd_fn) -> cmd_fn runtime_state
        | _ -> return runtime_state ) )

let rec prompt runtime_state =
  flush_all () ;
  CPrinter.cprint ~color:BrightGreen ~format:Bold "> " ;
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

and runtime options filename =
  try
    set_logger options.logger_level
    >>= fun _ ->
    input_file filename
    >>= fun _ ->
    parse_program_from_file filename
    >>= fun program ->
    Logger.success "Parsed successfully" ;
    preprocess_program program
    >>= fun (event_env, expr_env, program) ->
    (* Logger.success "Preprocessed successfully" ; *)
    Logger.debug @@ "Expr Env after preprocessing:\n"
    ^ string_of_env unparse_expr expr_env ;
    Logger.debug @@ "Event Env after preprocessing:\n"
    ^ string_of_env (fun e -> unparse_events [e]) event_env ;
    typecheck ~event_env program
    >>= fun (ty_env, event_env) ->
    Logger.success "Typechecked successfully" ;
    instantiate ~expr_env ~event_env program
    >>= fun (program, event_env, expr_env) ->
    Logger.success "Instantiated successfully" ;
    Logger.debug @@ "Expr Env after instantiation:\n"
    ^ string_of_env unparse_expr expr_env ;
    Logger.debug @@ "Event Env after instantiation:\n"
    ^ string_of_env (fun e -> unparse_events [e]) event_env ;
    let runtime_state = mk_runtime_state ~ty_env ~expr_env ~event_env program in
    prompt runtime_state
  with Duplicate_binding id ->
    Logger.error @@ "Duplicate binding: " ^ id ;
    return empty_runtime_state

let term = Term.(const runtime $ options $ input_filename)

let cmd = Cmd.v info term
