open State
open Common
open Printing

let commands =
  let visible =
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
  let visible =
    create_cmd
      ("help", [], "Prints the list of available commands.")
      (Help.term visible) visible
  in
  create_cmd
    ("debug", [], "Prints the current state of the graph.")
    Debug.term visible
  |> create_cmd
       ("rollback", [], "Rollbacks a number of times in the program.")
       Rollback.term

let cmds_bbk_tree = Bktree.create @@ List.map (fun (name, _) -> name) commands
