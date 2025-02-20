open Common
open Monads.ResultMonad
open Repl.State
open Options
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

let start_repl filename options =
  let* _ = set_logger options.logger_level in
  Repl.Main.runtime filename

let term = Term.(const start_repl $ input_filename $ options)

let cmd = Cmd.v info term

let run () =
  match Cmd.eval_value cmd with
  | Ok (`Ok result) ->
      result |> print_output ~previous_state:empty_runtime_state |> ignore
  | Error _ -> ()
  | _ -> ()
