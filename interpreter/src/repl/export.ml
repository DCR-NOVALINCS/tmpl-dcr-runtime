open State
open Errors
open Core
open Api
open Common
open Monads.ResultMonad
open Printing
open Cmdliner

(** [filenames] a list of filenames to export the program to.
    @return a argument of the list of filenames *)
let rec filenames =
  Arg.(
    non_empty & pos_all string []
    & info [] ~docv:"FILENAME" ~doc:"The filename to export the program to" )

(** [available_modes] a list of available modes to export the program to. *)
and available_modes = ["tdcr"; "json"; "dot"]

(** [modes] a list of modes to export the program to.
    @return a argument of the list of modes *)
and modes =
  Arg.(
    value & opt_all string ["tdcr"]
    & info ["m"; "mode"] ~docv:"MODE" ~doc:"The mode to export the program to" )

(** [export_cmd] command to export the program to the file(s).
    @param filenames a list of filenames to export the program to
    @param modes a list of modes to export the program to
    @param state the current state
    @return the new state with the output *)
and export_cmd filenames modes state =
  let selected_modes =
    List.filter (fun mode -> List.mem mode available_modes) modes
  and export_to mode program =
    let unparse_fn =
      match mode with
      | "tdcr" -> unparse_program_tdcr program
      | "json" -> unparse_program_json program
      | "dot" -> unparse_program_dot program
      | _ -> invalid_export_mode mode
    in
    unparse_fn
  and write_to_file unparsed_program filename =
    let oc = open_out filename in
    Printf.fprintf oc "%s\n" unparsed_program ;
    close_out oc ;
    return ()
  in
  let* _ =
    iter
      (fun filename ->
        map
          (fun mode ->
            export_to mode state.program
            >>= fun unparsed_program ->
            return (Printf.sprintf "%s.%s" filename mode, unparsed_program) )
          selected_modes
        >>= fun unparsed_programs ->
        iter
          (fun (filename, unparsed_program) ->
            write_to_file unparsed_program filename )
          unparsed_programs )
      filenames
  in
  return
    { state with
      output=
        Printf.sprintf "Exported program to file(s): %s"
          (String.concat ", " (List.map keyword filenames)) }

let term = Term.(const export_cmd $ filenames $ modes)
