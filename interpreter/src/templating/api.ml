open Syntax
open Lex_and_parse
open Evaluation
open Errors
open Runtime
open Program_helper
open Typechecking
open Misc
open Monads.ResultMonad
open Env
open Printing
open Cmdliner

(* =============================================================================
   Available functions
   ============================================================================= *)

let rec execute ~event_id ?(expr = Unit) ?(ty_env = empty_env)
    ?(expr_env = empty_env) ?(event_env = empty_env) program =
  (* preprocess_program program >>= fun (event_env, expr_env, program) -> *)
  match find_flat event_id event_env with
  | None -> event_not_found event_id
  | Some event ->
      (* Check if the event is enabled *)
      is_enabled event program (event_env, expr_env)
      >>= fun is_enabled ->
      if not is_enabled then event_not_enabled event
      else
        (* Executing the event according to its kind (i.e input or output) *)
        let io = event.data.io in
        ( match io.data with
        | Input _ -> execute_input_event event expr (ty_env, expr_env)
        | Output _ -> execute_output_event event expr_env )
        >>= fun event ->
        (* Update marking *)
        set_marking ~executed:true event
        >>= fun event ->
        (* Propagate relation effects that this event is apart of. *)
        propagate_effects event (event_env, expr_env)
          (update_event event program)

and execute_output_event event expr_env =
  let {info; io; _} = event.data in
  ( match io.data with
  | Output expr -> return expr
  | _ ->
      let id, _ = info in
      something_went_wrong ~loc:id.loc
        ("Is not a output event" ^ CString.colorize ~color:Yellow id.data) )
  >>= fun expr ->
  eval_expr expr expr_env >>= fun value -> set_marking ~value event

and execute_input_event event expr (ty_env, expr_env) =
  let {info; io; _} = event.data in
  match io.data with
  | Input expected_ty ->
      eval_expr (annotate expr) expr_env
      >>= fun value ->
      typecheck_expr ~ty_env value
      >>= fun ty ->
      if not (equal_types ty expected_ty.data) then
        type_mismatch ~loc:expected_ty.loc [expected_ty.data] [ty]
      else set_marking ~value event
  | _ ->
      let id, _ = info in
      something_went_wrong ~loc:id.loc
        ("Is not a input event" ^ CString.colorize ~color:Yellow id.data)

(* --- Unparse --- *)

and unparse_program program =
  let open Unparser.PlainUnparser in
  Ok (unparse program)

(* --- Vizualization functions --- *)

and view ?(filter = fun _ event -> Some event) ?(should_print_events = true)
    ?(should_print_relations = false) ?(expr_env = empty_env)
    ?(event_env = empty_env) program =
  filter_map (fun event -> filter (event_env, expr_env) event) program.events
  >>= fun events ->
  let open Unparser.PlainUnparser in
  return
    (unparse ~should_print_events ~should_print_value:true
       ~should_print_executed_marking:true ~should_print_relations
       ~should_print_template_decls:false {program with events} )

and view_debug program =
  let open Unparser.PlainUnparser in
  return @@ unparse ~should_print_executed_marking:true program
(* view ~should_print_relations:true program *)

and view_enabled ?(should_print_relations = false) program =
  view ~should_print_relations
    ~filter:(fun (event_env, expr_env) event ->
      is_enabled event program (event_env, expr_env)
      |> function Ok true -> Some event | _ -> None )
    program

and _view_disabled ?(should_print_relations = false) program =
  view ~should_print_relations
    ~filter:(fun (event_env, expr_env) event ->
      is_enabled event program (event_env, expr_env)
      |> function Ok false -> Some event | _ -> None )
    program

(* =============================================================================
   Runtime State Management Section
   ============================================================================= *)

type runtime_state =
  { ty_env: type_expr' env
  ; expr_env: expr env
  ; event_env: event env
  ; program: program
  ; output: string }

let mk_runtime_state ?(output = "") ?(ty_env = empty_env)
    ?(expr_env = empty_env) ?(event_env = empty_env) program =
  {ty_env; expr_env; event_env; program; output}

let empty_runtime_state =
  mk_runtime_state ~output:"" ~ty_env:empty_env ~expr_env:empty_env
    ~event_env:empty_env empty_program

(* =============================================================================
   Available main commands
   ============================================================================= *)

type cmd_callback =
  runtime_state -> (runtime_state, detailed_error list) result Cmd.t

let create_cmd (name, alias) term cmds =
  let cmd = Cmd.v (Cmd.info name) term in
  (name, cmd) :: (alias, cmd) :: cmds

and quit_term =
  let quit_cmd _ = exit 0 in
  Term.(const quit_cmd $ const empty_runtime_state)

and help_term =
  let help_cmd _ =
    let header = CString.colorize ~color:BrightCyan "Available Commands:" in
    let cmds_section =
      [ ("exit (q)", [], "Quit the program")
      ; ("help (h)", [], "Show this help message")
      ; ("view (v)", [], "View the current program")
      ; ( "execute (e)"
        , ["<event_id>"; "<expr>"]
        , "Execute an event with an expression" ) ]
      (* |> List.map (fun (name, desc) -> Printf.sprintf "%-20s %s" name desc) *)
      |> List.map (fun (name, params, desc) ->
             Printf.sprintf "- %s %-15s %s"
               (CString.colorize ~color:Green name)
               (String.concat ", "
                  (List.map (CString.colorize ~color:Red) params) )
               desc )
      |> String.concat "\n"
    in
    Printf.printf "%s" @@ String.concat "\n" [header; cmds_section] ;
    return empty_runtime_state
  in
  Term.(const help_cmd $ const empty_runtime_state)

and view_term =
  let all_flag =
    Arg.(
      value & opt bool false & info ["all"] ~docv:"ALL" ~doc:"View all events" )
  and view_cmd is_all =
    match is_all with
    | true ->
        CPrinter.cprintln ~color:Magenta "Viewing all events" ;
        return empty_runtime_state
    | false ->
        CPrinter.cprintln ~color:Magenta "Viewing enabled events" ;
        return empty_runtime_state
  in
  Term.(const view_cmd $ all_flag)

and execute_term =
  let parse_expression expr_string =
    (* let expr = expr_string |> String.concat " " in *)
    let expr_lexbuf = Lexing.from_string expr_string in
    Logger.debug @@ "Parsing expression " ^ expr_string ;
    parse_expression expr_lexbuf
  in
  let event_id =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"EVENT_ID" ~doc:"The event id to execute" )
  and expr =
    Arg.(
      required
      & pos 1 (some string) None
      & info [] ~docv:"EXPR" ~doc:"The expression to execute" )
  and execute_cmd event_id expr_str =
    parse_expression expr_str
    >>= fun expr ->
    CPrinter.cprintln ~color:Magenta "Executing event" ;
    execute ~event_id ~expr:expr.data empty_program
    >>= fun (program, event_env, expr_env) ->
    return (mk_runtime_state ~ty_env:empty_env ~expr_env ~event_env program)
  in
  Term.(const execute_cmd $ event_id $ expr)

let cmds =
  create_cmd ("exit", "q") quit_term []
  |> create_cmd ("help", "h") help_term
  |> create_cmd ("view", "v") view_term
  |> create_cmd ("execute", "e") execute_term

let cmds_bbk_tree = Misc.Bktree.create @@ List.map (fun (name, _) -> name) cmds
