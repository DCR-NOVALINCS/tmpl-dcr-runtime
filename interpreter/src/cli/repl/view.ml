open State

(* open Errors *)
open Core.Api
open Common
open Monads.ResultMonad
open Printing
open Cmdliner

let show_value =
  Arg.(
    value & flag
    & info ["v"; "value"] ~docv:"VALUE" ~doc:"Show the value of the events" )

let disabled =
  Arg.(
    value & flag
    & info ["d"; "disabled"] ~docv:"DISABLED" ~doc:"View the disabled events" )

let show_relations =
  Arg.(
    value & flag
    & info ["r"; "relations"] ~docv:"RELATIONS"
        ~doc:"Show the relations between the events" )

let show_template_defs =
  Arg.(
    value & flag
    & info ["t"; "templates"] ~docv:"TEMPLATES"
        ~doc:"Show the template definitions, only when the -a flag is active" )

and view_cmd show_value disabled show_relations show_template_defs
    {program; ty_env; event_env; expr_env; _} =
  let filter program =
    match disabled with
    | false ->
        view_enabled ~should_print_value:show_value
          ~should_print_relations:show_relations
          ~should_print_template_decls:show_template_defs ~event_env ~expr_env
          program
    | true ->
        view_disabled ~should_print_value:show_value
          ~should_print_relations:show_relations
          ~should_print_template_decls:show_template_defs ~event_env ~expr_env
          program
    (* | true, false ->
        view
          ~filter:(fun event _ -> Some event)
          ~should_print_events:true ~should_print_value:show_value
          ~should_print_relations:show_relations
          ~should_print_template_decls:show_template_defs ~event_env ~expr_env
          program *)
    (* | _ -> invalid_view_combination "all" "disabled" *)
  in
  filter program
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
    const view_cmd $ show_value $ disabled $ show_relations $ show_template_defs )
