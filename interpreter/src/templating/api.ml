open Syntax
open Evaluation
open Errors
open Runtime
open Program_helper
open Typechecking
open Misc.Monads.ResultMonad
open Misc.Env
open Misc.Printing

(* =============================================================================
   Available main commands
   ============================================================================= *)

type cmd_description =
  {name: string; alias: string; params: string list; desc: string}

let cmds =
  [ {name= "view"; alias= "v"; params= []; desc= "View the current program"}
  ; { name= "debug"
    ; alias= "d"
    ; params= []
    ; desc= "View the current program with relations" }
  ; { name= "exec"
    ; alias= "e"
    ; params= ["event_id"; "[expr]"]
    ; desc= "Execute an event with an expression" }
  ; { name= "parse"
    ; alias= "p"
    ; params= ["filename"]
    ; desc= "Parse a file and update the current program" }
  ; { name= "export"
    ; alias= "exp"
    ; params= ["filename"]
    ; desc= "Export the current program to a file" }
  ; {name= "exit"; alias= "q"; params= []; desc= "Exit the program"}
  ; {name= "help"; alias= "h"; params= []; desc= "Display this message"} ]

let cmds_bbk_tree =
  Misc.Bktree.create
  @@ (List.map (fun {name; alias; _} -> [name; alias]) cmds |> List.flatten)

(* =============================================================================
   Available functions
   ============================================================================= *)

let rec execute ~event_id ?(expr = Unit) program =
  preprocess_program program
  >>= fun (event_env, expr_env, program) ->
  match find_flat event_id event_env with
  | None -> event_not_found event_id
  | Some event ->
      is_enabled event program (event_env, expr_env)
      >>= fun is_enabled ->
      if not is_enabled then event_not_enabled event
      else
        let io = event.data.io in
        ( match io.data with
        | Input _ -> execute_input_event event expr expr_env
        | Output _ -> execute_output_event event expr_env )
        >>= fun event ->
        set_marking ~marking:(mk_marking ~executed:true ()) event
        >>= fun event ->
        propagate_effects event (event_env, expr_env)
          (update_event event program)

(* is_enabled event program (event_env, expr_env) |> function | false ->
   event_not_enabled event | true -> let io = event.data.io in ( match io.data
   with | Input _ -> execute_input_event event expr expr_env | Output _ ->
   execute_output_event event expr_env ) >>= fun event -> propagate_effects
   event (event_env, expr_env) (update_event event program) ) *)

(* and execute_event event expr env = let loc = match event.data.io.data with |
   Input _ -> event.data.io.loc | Output expr -> expr.loc in let expr = annotate
   ~loc expr in eval_expr expr env >>= fun expr -> set_marking
   ~marking:(mk_marking ~executed:true ~value:expr.data ()) event () *)

and execute_output_event event env =
  let something_went_wrong message event =
    let id, _ = event.data.info in
    fixme
    @@ Printf.sprintf "%s %s" message
    @@ CString.colorize ~color:Yellow id.data
  in
  ( match event.data.io.data with
  | Output expr -> return expr
  | _ -> something_went_wrong "Is not a output event" event )
  >>= fun expr ->
  eval_expr expr env
  >>= fun value -> set_marking ~marking:(mk_marking ~value:value.data ()) event

and execute_input_event event expr expr_env =
  let something_went_wrong message event =
    let id, _ = event.data.info in
    fixme
    @@ Printf.sprintf "%s %s" message
    @@ CString.colorize ~color:Yellow id.data
  in
  match event.data.io.data with
  | Input expected_ty ->
      eval_expr (annotate expr) expr_env
      >>= fun value ->
      typecheck_expr value
        empty_env (* FIXME: Accumulate an environment of types *)
      >>= fun ty ->
      if not (equal_types ty expected_ty.data) then
        type_mismatch [expected_ty.data] [ty]
      else set_marking ~marking:(mk_marking ~value:value.data ()) event
  | _ -> something_went_wrong "Is not a input event" event

and preprocess_program ?(expr_env = empty_env) ?(event_env = empty_env) program
    =
  (* Evaluate the value inside of the events *)
  let events = program.events in
  (* map (fun event -> update_event_value event expr_env) events >>= fun events
     -> *)
  (* Add all events as value into event environment *)
  fold_left
    (fun event_env event ->
      let id, _ = event.data.info in
      return (bind id.data event event_env) )
    event_env events
  >>= fun event_env -> return (event_env, expr_env, program)

(* --- Unparse --- *)

and unparse_program program =
  let open Unparser.PlainUnparser in
  Ok (unparse program)

(* --- Vizualization functions --- *)

and view ?(filter = fun _ event -> Some event) ?(should_print_events = true)
    ?(should_print_relations = false) program =
  preprocess_program program
  >>= fun (event_env, expr_env, program) ->
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
