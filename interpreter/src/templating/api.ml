open Syntax
open Misc.Monads
open Misc.Env
open Misc.Printing
open Evaluation
open Errors
open Runtime

(*
===============================================================
  Available functions
===============================================================
*)

let rec execute 
  ~event_id ?(expr = Unit) 
  (* ?(event_env = empty_env)  *)
  (* ?(expr_env = empty_env)  *)
  program  =
  preprocess_program program
  >>= fun (event_env, expr_env) ->
  match find_flat event_id event_env with
  | None -> event_not_found event_id
  | Some event ->
    is_enabled event program (event_env, expr_env)
    |> function
    | false -> event_not_enabled event
    | true ->
      let io = event.data.io in
      begin match io.data with
        | Input _ -> execute_event event expr expr_env
        | Output data_expr -> execute_event event data_expr.data expr_env (* [expr] is ignored *)
      end
      >>= fun event ->
      propagate_effects event (event_env, expr_env) (update_event event program)

and execute_event event expr env =
  let loc = match event.data.io.data with
  | Input _ -> event.data.io.loc
  | Output expr -> expr.loc in
  let expr = annotate ~loc expr in
  eval_expr expr env
  >>= fun expr ->
  let marking = event.data.marking in
  set_marking ~marking:(mk_marking ~executed:true ~included:marking.data.included.data ~pending:marking.data.pending.data ~value:expr.data ()) event
  (* let marking = { event.data.marking with data = { event.data.marking.data with value = expr; executed = annotate true } } in
  let event = { event with data = { event.data with marking } } in
  Ok event *)

and preprocess_program ?(expr_env = empty_env) program =
  fold_left_result
    (fun env event ->
      let (id , _) = event.data.info in
      Ok (bind id.data event env))
    expr_env program.events
  >>= fun event_env ->

  fold_left_result
    (fun env event ->
      let (id, _) = event.data.info in
      Ok (bind id.data (record_event event) env))
    empty_env program.events
  >>= fun expr_env ->

  Ok (event_env, expr_env)

(* --- Vizualization functions --- *)

and view
  ?(filter = (fun _ _ -> true))
  (* ?(event_env = empty_env)
  ?(expr_env = empty_env) *)
  ?(should_print_events = true)
  ?(should_print_relations = false)
  program =
  preprocess_program program
  >>= fun (event_env, expr_env) ->
  ( if not should_print_events then ""
  else
    List.filter (filter (event_env, expr_env)) program.events
    |> List.sort compare
    |> List.map (fun event -> string_of_event event)
    |> String.concat "\n"
  )
  |> fun events_str ->
  ( if not should_print_relations then events_str
  else
      List.map (fun relation -> string_of_relation relation) program.relations
      |> String.concat "\n"
      |> Printf.sprintf "%s\n;\n%s" events_str
  )
  |> Result.ok

and view_debug program =
    let open Unparser in
    Ok (unparse program)
  (* view ~should_print_relations:true program *)

and view_enabled
  (* ?(event_env = empty_env)
  ?(expr_env = empty_env) *)
  ?(should_print_relations = false)
  program =
  view ~should_print_relations ~filter:(fun (event_env, expr_env) event ->
    is_enabled event program (event_env, expr_env)) program

and _view_disabled program =
  view ~filter:(fun (event_env, expr_env) event ->
    not (is_enabled event program (event_env, expr_env))) program
