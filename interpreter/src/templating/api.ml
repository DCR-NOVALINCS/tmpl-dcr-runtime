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
