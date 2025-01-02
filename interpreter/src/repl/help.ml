open State
(* open Core *)

(* open Api
   open Ast.Unparser *)
open Common
open Monads.ResultMonad
open Printing
open Cmdliner

let help_cmd commands state =
  let header = CString.colorize ~color:BrightCyan "Available Commands:" in
  let cmds_section =
    commands
    |> List.map (fun (name, {params; description; _}) ->
           Printf.sprintf "- %s %s: %-15s"
             (CString.colorize ~color:Green name)
             (String.concat " "
                (List.map (fun s -> keyword (Printf.sprintf "<%s>" s)) params) )
             description )
    |> String.concat "\n"
  in
  return {state with output= Printf.sprintf "%s\n%s" header cmds_section}

let term commands = Term.(const help_cmd $ const commands)
