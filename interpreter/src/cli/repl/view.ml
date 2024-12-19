open State
open Core.Api
open Common
open Monads.ResultMonad
open Printing
open Cmdliner

let all_flag =
  Arg.(value & flag & info ["a"; "all"] ~docv:"ALL" ~doc:"View the whole graph")

and show_value =
  Arg.(
    value & flag
    & info ["v"; "value"] ~docv:"VALUE" ~doc:"Show the value of the events" )

and disabled =
  Arg.(
    value & flag
    & info ["d"; "disabled"] ~docv:"DISABLED" ~doc:"View the disabled events" )

and show_relations =
  Arg.(
    value & flag
    & info ["r"; "relations"] ~docv:"RELATIONS"
        ~doc:"Show the relations between the events" )

and show_template_defs =
  Arg.(
    value & flag
    & info ["t"; "templates"] ~docv:"TEMPLATES"
        ~doc:"Show the template definitions, only when the -a flag is active" )

and view_cmd is_all show_value disabled show_relations show_template_defs
    {program; ty_env; event_env; expr_env; _} =
  let filter =
    match (is_all, disabled) with
    | false, false ->
        view_enabled ~should_print_value:show_value
          ~should_print_relations:show_relations
    | false, true ->
        view_disabled ~should_print_value:show_value
          ~should_print_relations:show_relations
    | true, false ->
        view
          ~filter:(fun event _ -> Some event)
          ~should_print_events:true ~should_print_value:show_value
          ~should_print_relations:show_relations
          ~should_print_template_decls:show_template_defs
    | _ -> raise (Invalid_argument "view_cmd")
  in
  filter ~event_env ~expr_env program
  >>= fun output ->
  let has_events_to_show output =
    if String.trim output = "" then
      CString.colorize ~format:Italic "No events to show."
    else output
  in
  return
    {program; ty_env; event_env; expr_env; output= has_events_to_show output}

let term =
  Term.(
    const view_cmd $ all_flag $ show_value $ disabled $ show_relations
    $ show_template_defs )
