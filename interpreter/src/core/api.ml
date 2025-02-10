(** {1 API}

    This module provides functions to interact with the interpreter. *)

open Ast
open Syntax
open Error
open Parsing.Lex_and_parse
open Dcr
open Evaluation
open Transition
open Instantiation
open Errors
open Helper
open Typing
open Helper
open Typechecking
open Errors
open Common
open Monads.ResultMonad
open Env
open Printing

(* =============================================================================
   Initialization functions
   ============================================================================= *)

let initialize program =
  let* event_env, expr_env, program = preprocess_program program in
  let* ty_env, event_env, label_types = typecheck ~event_env program in
  Logger.success "Typechecked successfully" ;
  let* program, event_env, expr_env =
    instantiate ~expr_env ~event_env program
  in
  Logger.success "Instantiated successfully" ;
  return (program, (ty_env, label_types, expr_env, event_env))

(* =============================================================================
   Execute functions
   ============================================================================= *)

let rec execute ~event_id ?(expr = Unit) ?(ty_env = empty_env)
    ?(label_types = EventTypes.empty) ?(expr_env = empty_env)
    ?(event_env = empty_env) program =
  let* _ = typecheck_expr ~ty_env ~label_types (annotate expr) in
  match find_flat event_id event_env with
  | None -> event_not_found event_id
  | Some event ->
      (* Check if the event is enabled *)
      let* is_enabled = is_enabled event program (event_env, expr_env) in
      if not is_enabled then event_not_enabled event
      else
        (* Executing the event according to its kind (i.e input or output) *)
        let io = event.data.io in
        let* event =
          ( match io.data with
          | Input _ -> execute_input_event event expr (ty_env, expr_env)
          | Output _ -> execute_output_event event expr_env )
          (* Update marking *)
          >>= set_marking ~executed:true
        in
        let program = update_event event program in
        (* Propagate relation effects that this event is apart of. *)
        let* program, _, _ =
          propagate_effects event (event_env, expr_env) program
        in
        let* event_env, expr_env, program = preprocess_program program in
        let* ty_env, event_env, label_types = typecheck ~event_env program in
        return (program, event_env, expr_env, ty_env, label_types)

and execute_output_event event expr_env =
  let {info; io; marking; _} = event.data in
  let* expr =
    match io.data with
    | Output expr -> return expr
    | _ ->
        let id, _ = info in
        something_went_wrong ~loc:id.loc
          ("Is not a output event" ^ keyword id.data)
  in
  let* v = eval_expr expr expr_env in
  let {value; _} = marking.data in
  value := v ;
  set_marking ~value:v event

and execute_input_event event expr (ty_env, expr_env) =
  let {info; io; _} = event.data in
  match io.data with
  | Input expected_ty ->
      let* value = eval_expr (annotate expr) expr_env in
      let* ty = typecheck_expr ~ty_env value in
      if not (equal_types ty expected_ty.data) then
        type_mismatch ~loc:expected_ty.loc [expected_ty.data] [ty]
      else set_marking ~value event
  | _ ->
      let id, _ = info in
      something_went_wrong ~loc:id.loc ("Is not a input event" ^ keyword id.data)

(* =============================================================================
   Parsing functions
   ============================================================================= *)

and parse_program_from_file filename =
  let file_channel = open_in filename in
  let lexbuf = Lexing.from_channel file_channel in
  parse_program ~filename lexbuf
  >>| fun program -> close_in file_channel ; program

and parse_expression_from_string expr_tokens =
  (* sanitize_input expr_string
     >>= fun expr_string_tokens -> *)
  if List.is_empty expr_tokens then return (annotate Unit)
  else
    let lexbuf = Lexing.from_string (String.concat " " expr_tokens) in
    parse_expression lexbuf

(* =============================================================================
   Unparsing functions
   ============================================================================= *)

and unparse_program_tdcr ?(should_print_value = false)
    ?(should_print_executed_marking = false) program =
  let open Unparser in
  return
    (Plain.unparse ~should_print_executed_marking ~should_print_value program)

and unparse_program_json program =
  let open Yojson.Safe in
  return (pretty_to_string (yojson_of_program program))

and unparse_program_dot program =
  let open Unparser in
  return (Dot.unparse program)

(* =============================================================================
   Visualization functions
   ============================================================================= *)

and view ?(filter = fun event _ -> Some event)
    ?(should_print_template_decls = false) ?(should_print_events = true)
    ?(should_print_value = false) ?(should_print_relations = false)
    ?(expr_env = empty_env) ?(event_env = empty_env) program =
  filter_map (fun event -> filter event (event_env, expr_env)) program.events
  >>= fun events ->
  let* program = sort_events {program with events} in
  let open Unparser in
  return
    (Colorized.unparse ~should_print_events ~should_print_value
       ~should_print_executed_marking:true ~should_print_relations
       ~should_print_template_decls program )

and view_debug program =
  let open Unparser in
  return @@ Plain.unparse ~should_print_executed_marking:true program
(* view ~should_print_relations:true program *)

and view_enabled ?(should_print_template_decls = false)
    ?(should_print_value = false) ?(should_print_relations = false)
    ?(expr_env = empty_env) ?(event_env = empty_env) program =
  view ~should_print_template_decls ~should_print_value ~should_print_relations
    ~event_env ~expr_env
    ~filter:(fun event (event_env, expr_env) ->
      is_enabled event program (event_env, expr_env)
      |> function Ok true -> Some event | _ -> None )
    program

and view_disabled ?(should_print_template_decls = false)
    ?(should_print_value = false) ?(should_print_relations = false)
    ?(expr_env = empty_env) ?(event_env = empty_env) program =
  view ~should_print_template_decls ~should_print_relations ~should_print_value
    ~event_env ~expr_env
    ~filter:(fun event (event_env, expr_env) ->
      is_enabled event program (event_env, expr_env)
      |> function Ok false -> Some event | _ -> None )
    program
