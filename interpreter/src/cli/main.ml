open Common
open Monads.ResultMonad
open Printing
open Repl.State
open Cmdliner

let run () =
  match Cmd.eval_value Runtime.cmd with
  | Ok (`Ok result) ->
      ignore
        (let* state = result in
         return @@ CPrinter.cprintln ~format:Bold (string_of_state state) )
  | Error _ -> ()
  | _ -> ()
