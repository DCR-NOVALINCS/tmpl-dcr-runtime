(** {1 API}

    This module provides functions to interact with the interpreter. *)

open Ast
open Syntax
open Error
open Unparser
open Parsing.Lex_and_parse
open Dcr
open Evaluation
open Runtime
open Errors
open Helper
open Typing
open Typechecking
open Errors
open Common
open Monads.ResultMonad
open Env
open Printing

(* =============================================================================
   Execute functions
   ============================================================================= *)

let rec execute ~event_id ?(expr = Unit) ?(ty_env = empty_env)
    ?(expr_env = empty_env) ?(event_env = empty_env) program =
  typecheck_expr ~ty_env (annotate expr)
  >>= fun _ ->
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
        >>= (* Update marking *)
        set_marking ~executed:true
        >>= fun event ->
        return (update_event event program)
        >>= fun program ->
        Logger.debug @@ "Updated events:\n"
        ^ Plain.unparse_events program.events ;
        (* Debug event_env *)
        Logger.debug @@ "Event Env after executing event:\n"
        ^ string_of_env (fun e -> Plain.unparse_events [e]) event_env ;
        (* Propagate relation effects that this event is apart of. *)
        propagate_effects event (event_env, expr_env) program
        >>= fun (program, _, _) ->
        preprocess_program program
        >>= fun (event_env, expr_env, program) ->
        return (program, event_env, expr_env)

and execute_output_event event expr_env =
  let {info; io; _} = event.data in
  ( match io.data with
  | Output expr -> return expr
  | _ ->
      let id, _ = info in
      something_went_wrong ~loc:id.loc
        ("Is not a output event" ^ keyword id.data) )
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
    (Plain.unparse ~should_print_events ~should_print_value
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
