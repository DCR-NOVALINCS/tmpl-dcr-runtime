open Cmdliner
open State
open Ast
open Error
open Common
open Printing

type cmd =
  { params: string list
  ; description: string
  ; callback:
      (runtime_state -> (runtime_state, detailed_error list) result) Cmd.t }

let create_cmd (name, params, description) term cmds =
  (* Add both the default and shortened versions of the command to the list *)
  let cmd =
    {params; description; callback= Cmd.v (Cmd.info ~man:[] name) term}
  in
  (name, cmd) :: cmds

let cmds =
  let available_cmds =
    create_cmd ("exit", [], "Exit the program.") Quit.term []
    |> create_cmd
         ("view", [], "Views the current state of the graph.")
         View.term
    |> create_cmd
         ( "execute"
         , ["EVENT_ID"; "EXPR_STRING"]
         , "Executes the event " ^ keyword "<EVENT_ID>"
           ^ " with the expression " ^ keyword "<EXPR_STRING>" ^ " if needed."
         )
         Execute.term
    |> create_cmd
         ( "export"
         , ["FILENAME"]
         , "Creates a file named " ^ keyword "<FILENAME>"
           ^ " with a textual representation of the current state of the graph."
         )
         Export.term
  in
  create_cmd ("help", [], "Displays this message") Help.term available_cmds
  (* "Invisible" commands, only for debugging and testing *)
  |> create_cmd
       ("debug", [], "Shows any debug information available.")
       Debug.term

let cmds_bbk_tree = Bktree.create @@ List.map (fun (name, _) -> name) cmds
