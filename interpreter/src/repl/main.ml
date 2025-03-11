open State
open Errors
open Common
open Monads.ResultMonad
open Printing
open Env
open Checkable
open Core
open Api
open Cmdliner

(** [commands] is a list of all available commands in the program.
    @return
      a tuple of list of all available commands and a BK-tree of the commands.
*)
let commands =
  let visible =
    create_cmd ("exit", [], "Exit the program.") Quit.term []
    |> create_cmd
         ( "export"
         , ["FILENAME"]
         , "Creates a file named " ^ keyword "FILENAME"
           ^ " with a textual representation of the current state of the graph."
         )
         Export.term
    |> create_cmd
         ( "execute"
         , ["EVENT_ID"; "EXPR_STRING"]
         , "Executes the event " ^ keyword "EVENT_ID" ^ " with the expression "
           ^ keyword "EXPR_STRING" ^ " if needed." )
         Execute.term
    |> create_cmd
         ("view", [], "Views the current state of the graph.")
         View.term
  in
  let all =
    create_cmd
      ("help", [], "Prints the list of available commands.")
      (Help.term visible) visible
    |> create_cmd
         ("rollback", [], "Rollbacks a number of times in the program.")
         Rollback.term
  in
  let cmds_bbk_tree = Bktree.create @@ List.map (fun (name, _) -> name) all in
  ( create_cmd
      ("debug", [], "Prints the current state of the graph.")
      Debug.term all
  , cmds_bbk_tree )

let interpret_command tokens runtime_state =
  match tokens with
  | [] -> return runtime_state
  | name :: _ -> (
      let commands, cmds_bbk_tree = commands in
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
      let* tokens = sanitize_input input in
      let* state =
        interpret_command tokens runtime_state
        |> print_output ~previous_state:runtime_state
      in
      prompt {state with output= ""}

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

and runtime filename =
  try
    let* _ = input_file filename in
    let* program = parse_program_from_file filename in
    Logger.success "Parsed successfully" ;
    let* program, (ty_env, label_types, expr_env, event_env) =
      initialize program
    in
    let runtime_state =
      mk_runtime_state ~ty_env ~label_types ~expr_env ~event_env program
    in
    prompt runtime_state
  with Duplicate_binding id ->
    Logger.error @@ "Duplicate binding: " ^ id ;
    return empty_runtime_state
