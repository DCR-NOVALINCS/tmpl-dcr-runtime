open Api
open Syntax
open Errors
open Misc
open Monads.ResultMonad
open Printing

(* open Env *)
open Cmdliner

(* =============================================================================
   Available main commands
   ============================================================================= *)

type cmd =
  { params: string list
  ; description: string
  ; callback:
      (runtime_state -> (runtime_state, detailed_error list) result) Cmd.t }

let create_cmd (name, params, description) term cmds : (string * cmd) list =
  (* Add both the default and shortened versions of the command to the list *)
  let cmd =
    {params; description; callback= Cmd.v (Cmd.info ~man:[] name) term}
  in
  (name, cmd) :: cmds

and quit_term =
  let quit_cmd _ = exit 0 in
  Term.(const quit_cmd)

and help_term available_cmds =
  let help_cmd state =
    let header = CString.colorize ~color:BrightCyan "Available Commands:" in
    let cmds_section =
      available_cmds
      |> List.map (fun (name, {params; description; _}) ->
             Printf.sprintf "- %s %s: %-15s"
               (CString.colorize ~color:Green name)
               (String.concat " "
                  (List.map
                     (fun s ->
                       CString.colorize ~color:Red (Printf.sprintf "<%s>" s) )
                     params ) )
               description )
      (* [ ("exit (q)", [], "Quit the program")
         ; ("help (h)", [], "Show this help message")
         ; ("view (v)", [], "View the current program")
         ; ( "execute (e)"
           , ["<event_id>"; "<expr>"]
           , "Execute an event with an expression" ) ] *)
      (* |> List.map (fun (name, params, desc) ->
             Printf.sprintf "- %s %-15s %s"
               (CString.colorize ~color:Green name)
               (String.concat " "
                  (List.map (CString.colorize ~color:Red) params) )
               desc ) *)
      |> String.concat "\n"
    in
    return {state with output= Printf.sprintf "%s\n%s" header cmds_section}
  in
  Term.(const help_cmd)

and view_term =
  let all_flag =
    Arg.(value & flag & info ["a"; "all"] ~docv:"ALL" ~doc:"View all events")
  and view_cmd is_all {program; ty_env; event_env; expr_env; _} =
    match is_all with
    | true ->
        view ~expr_env ~event_env program
        >>= fun program_str ->
        return
          (mk_runtime_state ~ty_env ~expr_env ~event_env ~output:program_str
             program )
    | false ->
        view_enabled ~expr_env ~event_env program
        >>= fun program_str ->
        return
          (mk_runtime_state ~ty_env ~expr_env ~event_env ~output:program_str
             program )
  in
  Term.(const view_cmd $ all_flag)

and execute_term =
  let event_id =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"EVENT_ID" ~doc:"The event id to execute" )
  and expr =
    Arg.(
      value & opt string "" & info ["expr"] ~docv:"EXPR" ~doc:"The expression" )
  and execute_cmd event_id expr_str {program; ty_env; event_env; expr_env; _} =
    parse_expression_from_string expr_str
    >>= fun expr ->
    execute ~ty_env ~expr_env ~event_env ~event_id ~expr:expr.data program
    >>= fun (program, event_env, expr_env) ->
    return
      (mk_runtime_state ~ty_env ~expr_env ~event_env
         ~output:
           (Printf.sprintf "Executed event %s with expression %s\n"
              (CString.colorize ~color:Yellow event_id)
              (CString.colorize ~color:Yellow
                 (Unparser.PlainUnparser.unparse_expr expr) ) )
         program )
  in
  Term.(const execute_cmd $ event_id $ expr)

and export_term =
  let rec filenames =
    Arg.(
      non_empty & pos_all string []
      & info [] ~docv:"FILENAME" ~doc:"The filename to export the program to" )
  (* Available modes:
     - "tdcr" (default)
     - "json"
     - "dot"
  *)
  and available_modes = ["tdcr"; "json"; "dot"]
  and modes =
    Arg.(
      value & opt_all string ["tdcr"]
      & info ["m"; "mode"] ~docv:"MODE" ~doc:"The mode to export the program to" )
  and export_cmd filenames modes state =
    let selected_modes =
      List.filter (fun mode -> List.mem mode available_modes) modes
    and export_to mode program =
      let unparse_fn =
        match mode with
        | "tdcr" -> unparse_program_tdcr program
        | "json" -> unparse_program_json program
        (* | "dot" -> unparse_program_dot program *)
        | _ -> should_not_happen ~module_path:"cmds.ml" "export_cmd"
      in
      unparse_fn
    and write_to_file unparsed_program filename =
      let oc = open_out filename in
      Printf.fprintf oc "%s\n" unparsed_program ;
      close_out oc ;
      return ()
    in
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
    >>= fun _ ->
    return
      { state with
        output=
          Printf.sprintf "Exported program to file(s): %s"
            (String.concat ", "
               (List.map (CString.colorize ~color:Yellow) filenames) ) }
  in
  Term.(const export_cmd $ filenames $ modes)

let cmds =
  let available_cmds =
    create_cmd ("exit", [], "Exit the program.") quit_term []
    |> create_cmd
         ("view", [], "Views the current state of the graph.")
         view_term
    |> create_cmd
         ( "execute"
         , ["EVENT_ID"; "EXPR_STRING"]
         , "Executes the event [EVENT_ID] with the expression [EXPR_STRING], if needed."
         )
         execute_term
    |> create_cmd
         ( "export"
         , ["FILENAME"]
         , "Creates a file named FILENAME with a textual representation of the current state of the graph."
         )
         export_term
  in
  create_cmd
    ("help", [], "Displays this message")
    (help_term available_cmds) available_cmds

let cmds_bbk_tree = Misc.Bktree.create @@ List.map (fun (name, _) -> name) cmds
