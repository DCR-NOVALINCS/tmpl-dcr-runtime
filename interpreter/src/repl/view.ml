open State
open Core.Api
open Common
open Monads.ResultMonad
open Checkable
open Printing
open Cmdliner

let disabled =
  Arg.(
    value & flag
    & info ["d"; "disabled"] ~docv:"DISABLED" ~doc:"View the disabled events" )

let show_value =
  Arg.(
    value & flag
    & info ["v"; "value"] ~docv:"VALUE" ~doc:"Show the value of the events" )

let show_relations =
  Arg.(
    value & flag
    & info ["r"; "relations"] ~docv:"RELATIONS"
        ~doc:"Show the relations between the events" )

let show_template_defs =
  Arg.(
    value & flag
    & info ["t"; "templates"] ~docv:"TEMPLATES"
        ~doc:"Show the template definitions" )

and view_cmd disabled show_value show_relations show_template_defs state =
  let {expr_env; event_env; program; _} = state in
  let view program =
    match disabled with
    | false ->
        view_enabled ~should_print_value:show_value
          ~should_print_relations:show_relations
          ~should_print_template_decls:show_template_defs ~event_env ~expr_env
          program
    | true ->
        view
          ~filter:(fun event _ -> Some event)
          ~should_print_value:show_value ~should_print_relations:show_relations
          ~should_print_template_decls:show_template_defs ~event_env ~expr_env
          program
  in
  let* output = view program in
  let* output =
    is_empty_string
      ~map:(fun _ -> CString.colorize ~format:Italic "No events to show.")
      output
  in
  return {state with output}

let term =
  Term.(
    const view_cmd $ disabled $ show_value $ show_relations $ show_template_defs )
