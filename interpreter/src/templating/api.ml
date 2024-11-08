open Syntax
open Evaluation
open Errors
open Runtime
open Misc.Monads.ResultMonad
open Misc.Env
(* open Misc.Printing *)

(* ===============================================================
   Available main commands
   =============================================================== *)

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

(* ===============================================================
   Available functions
   =============================================================== *)

let rec execute ~event_id ?(expr = Unit)
    (* ?(event_env = empty_env)  *)
    (* ?(expr_env = empty_env)  *)
      program =
  preprocess_program program
  >>= fun (event_env, expr_env, program) ->
  match find_flat event_id event_env with
  | None -> event_not_found event_id
  | Some event -> (
      is_enabled event program (event_env, expr_env)
      |> function
      | false -> event_not_enabled event
      | true ->
          let io = event.data.io in
          ( match io.data with
          | Input _ -> execute_event event expr expr_env
          | Output data_expr ->
              execute_event event data_expr.data
                expr_env (* [expr] is ignored *) )
          >>= fun event ->
          propagate_effects event (event_env, expr_env)
            (update_event event program) )

and execute_event event expr env =
  let loc =
    match event.data.io.data with
    | Input _ -> event.data.io.loc
    | Output expr -> expr.loc
  in
  let expr = annotate ~loc expr in
  eval_expr expr env
  >>= fun expr ->
  let marking = event.data.marking in
  set_marking
    ~marking:
      (mk_marking ~executed:true ~included:marking.data.included.data
         ~pending:marking.data.pending.data ~value:expr.data () )
    event
(* let marking = { event.data.marking with data = { event.data.marking.data
   with value = expr; executed = annotate true } } in let event = { event
   with data = { event.data with marking } } in Ok event *)

and preprocess_program ?(expr_env = empty_env) program =
  (* Add all events as value into expr environment *)
  fold_left
    (fun env event ->
      let id, _ = event.data.info in
      Ok (bind id.data (record_event event) env) )
    expr_env program.events
  >>= fun expr_env ->
  let open Misc.Printing in
  Logger.debug
  @@ Printf.sprintf "Preprocess program: %s"
       (string_of_env Unparser.PlainUnparser.unparse_expr expr_env) ;
  (* Update the value of each event *)
  map
    (fun event ->
      let open Instantiation in
      replace_event event expr_env )
    program.events
  >>= fun events ->
  (* Add all events into event environment *)
  fold_left
    (fun env event ->
      let id, _ = event.data.info in
      Ok (bind id.data event env) )
    empty_env events
  >>= fun event_env -> Ok (event_env, expr_env, {program with events})

(* --- Unparse --- *)

and unparse_program program =
  let open Unparser in
  Ok (PlainUnparser.unparse program)

(* --- Vizualization functions --- *)

and view ?(filter = fun _ _ -> true) ?(should_print_events = true)
    ?(should_print_relations = false) program =
  preprocess_program program
  >>= fun (event_env, expr_env, program) ->
  let open Unparser in
  let events =
    List.filter
      (fun event -> filter (event_env, expr_env) event)
      program.events
  in
  let program = {program with events} in
  Ok
    (PlainUnparser.unparse ~should_print_events ~should_print_value:true
       ~should_print_executed_marking:true ~should_print_relations
       ~should_print_template_decls:false program )
(* preprocess_program program >>= fun (event_env, expr_env) -> ( if not
   should_print_events then "" else List.filter (filter (event_env,
   expr_env)) program.events |> List.sort compare |> List.map (fun event ->
   string_of_event event) |> String.concat "\n" ) |> fun events_str -> ( if
   not should_print_relations then events_str else List.map (fun relation
   -> string_of_relation relation) program.relations |> String.concat "\n"
   |> Printf.sprintf "%s\n;\n%s" events_str ) |> Result.ok *)

and view_debug program =
  let open Unparser in
  Ok (PlainUnparser.unparse program)
(* view ~should_print_relations:true program *)

and view_enabled
    ?((* ?(event_env = empty_env) ?(expr_env = empty_env) *)
      should_print_relations = false) program =
  view ~should_print_relations
    ~filter:(fun (event_env, expr_env) event ->
      is_enabled event program (event_env, expr_env) )
    program

(* and _view_disabled program = view ~filter:(fun (event_env, expr_env)
   event -> not (is_enabled event program (event_env, expr_env)))
   program *)
