open Ast
open Syntax
open Error
open Unparser
open Evaluation
open Errors
open Common
open Monads.ResultMonad
open Env
open Printing

(* =============================================================================
   Updating event functions
   ============================================================================= *)

(** [update_event event program] updates the [event] in the [program] by
    replacing the old event with the new one.
    @param event The new event to be updated.
    @param program The program where the event is to be updated.
    @return The updated program. *)
let rec update_event event program =
  let events =
    List.map
      (fun e ->
        let id, _ = e.data.info in
        let id', _ = event.data.info in
        if id.data = id'.data then event else e )
      program.events
  in
  {program with events}

(* =============================================================================
   Marking functions
    ============================================================================= *)

and set_marking ?included ?pending ?executed ?value event =
  let {marking; _} = event.data in
  let {included= i; pending= p; executed= e; value= v} = marking.data in
  return
  @@ mk_marking
       ~included:(Option.value ~default:i.data included)
       ~pending:(Option.value ~default:p.data pending)
       ~executed:(Option.value ~default:e.data executed)
       ~value:(Option.value ~default:!v value).data ()
  >>= fun new_marking ->
  let marking = {marking with data= new_marking} in
  let event = {event with data= {event.data with marking}} in
  return event

and get_marking event =
  let {marking= {data= marking; _}; _} = event.data in
  marking

and is_included event = get_marking event |> fun {included; _} -> included.data

and is_executed event = get_marking event |> fun {executed; _} -> executed.data

and is_pending event = get_marking event |> fun {pending; _} -> pending.data

and current_value event = get_marking event |> fun {value; _} -> !value

and update_event_io ?(eval = eval_expr) event expr_env =
  let {io; _} = event.data in
  match io.data with
  | Input _ ->
      let value = current_value event in
      eval value expr_env >>= fun value -> set_marking ~value event
  | Output expr ->
      eval expr expr_env
      >>= fun value ->
      set_marking ~value event
      >>= fun event ->
      return
        {event with data= {event.data with io= {io with data= Output value}}}

and update_event_info ?(eval = eval_expr) event expr_env =
  let {info= id, _; _} = event.data in
  eval (annotate ~loc:id.loc (Identifier id)) expr_env
  >>= fun id ->
  match id.data with
  | Identifier id -> set_info ~id:id.data event
  | _ -> return event

and set_info ?id ?label event =
  let e_id, e_label = event.data.info in
  return
    ( Option.value ~default:e_id.data id
    , Option.value ~default:e_label.data label )
  >>= fun (id', label') ->
  let info = ({e_id with data= id'}, {e_label with data= label'}) in
  return {event with data= {event.data with info}}

and update_event_env program event_env =
  let events = program.events in
  fold_left
    (fun event_env event ->
      let id, _ = event.data.info in
      return (bind id.data event event_env) )
    event_env events

(* =============================================================================
   Updating relation functions
   ============================================================================= *)

and change_relation old_id new_id relation =
  match relation.data with
  | ControlRelation (from, guard, dest, t) ->
      let new_from = if from.data = old_id.data then new_id else from in
      let new_dest = if dest.data = old_id.data then new_id else dest in
      {relation with data= ControlRelation (new_from, guard, new_dest, t)}
  | SpawnRelation (from, guard, subprogram) ->
      let new_from = if from.data = old_id.data then new_id else from in
      {relation with data= SpawnRelation (new_from, guard, subprogram)}

and update_relation_guard ?(eval = partial_eval_expr) relation expr_env =
  match relation.data with
  | ControlRelation (from, guard, dest, t) ->
      eval guard expr_env
      >>= fun guard_value ->
      return {relation with data= ControlRelation (from, guard_value, dest, t)}
  | _ -> return relation

and update_expr_id ?(eval = eval_expr) old_id new_id expr expr_env =
  eval expr expr_env
  >>= fun value ->
  match value.data with
  | Identifier id when id.data = old_id ->
      return {expr with data= Identifier new_id}
  | _ -> return expr

(* =============================================================================
   Preprocessing functions
   ============================================================================= *)

and preprocess_program ?(expr_env = empty_env) ?(event_env = empty_env) program
    =
  let events = program.events in
  (* Add all events as value into event environment *)
  fold_left
    (fun (event_env, expr_env) event ->
      let id, _ = event.data.info in
      return
        ( bind id.data event event_env
        , bind id.data (event_as_expr event) expr_env ) )
    (event_env, expr_env) events
  >>= fun (event_env, expr_env) -> return (event_env, expr_env, program)

and preprocess_subprogram ?(expr_env = empty_env) ?(event_env = empty_env)
    (events, template_insts, relations, annotations) =
  preprocess_program ~expr_env ~event_env
    {empty_program with events; template_insts; relations; annotations}
  >>= fun (event_env, expr_env, program) ->
  return
    ( event_env
    , expr_env
    , mk_subprogram ~events:program.events
        ~template_insts:program.template_insts ~relations:program.relations
        ~annotations:program.annotations () )

(* =============================================================================
   Getters
   ============================================================================= *)

(** [get_event ?filter program] returns the event with the identifier [id] in
    the [program] *)
and get_event ?(filter = fun _ -> true) program =
  let events = program.events in
  List.find_opt filter events

and has_event ?(filter = fun _ -> true) program =
  Option.is_some @@ get_event ~filter program

and find_event id event_env =
  match find_flat id.data event_env with
  | Some event -> return event
  | None -> event_not_found ~loc:id.loc id.data

and find_all_events ?(filter = fun _ -> true) program =
  let events = program.events in
  return @@ List.filter filter events

and same_id id e =
  let id', _ = e.data.info in
  id'.data |> String.split_on_char '_' |> List.hd = id

(* and is_executed id program =
     let event = get_event ~filter:(same_id id) program in
     match event with
     | None -> raise (Invalid_argument "Event not found")
     | Some event ->
         let {marking; _} = event.data in
         let {executed; _} = marking.data in
         executed.data

   and is_pending id program =
     let event = get_event ~filter:(same_id id) program in
     match event with
     | None -> raise (Invalid_argument "Event not found")
     | Some event ->
         let {marking; _} = event.data in
         let {pending; _} = marking.data in
         pending.data

   and is_included id program =
     let event = get_event ~filter:(same_id id) program in
     match event with
     | None -> raise (Invalid_argument "Event not found")
     | Some event ->
         let {marking; _} = event.data in
         let {included; _} = marking.data in
         included.data *)

and get_relation ?(filter = fun _ -> true) id program =
  let relations = program.relations in
  List.find_opt
    (fun r ->
      match r.data with
      | ControlRelation (from, _, dest, _) ->
          (from.data = id || dest.data = id) && filter r
      | SpawnRelation (from, _, _) -> from.data = id && filter r )
    relations

and is_spawn r = match r.data with SpawnRelation _ -> true | _ -> false

and is_ctrl op r =
  match r.data with ControlRelation (_, _, _, op') -> op = op' | _ -> false

and has_relation ?(filter = fun _ -> true) id program =
  Option.is_some @@ get_relation ~filter id program

and find_all_relations ?(filter = fun _ _ _ -> true) program =
  let relations = program.relations in
  return
  @@ List.filter
       (fun r ->
         match r.data with
         | ControlRelation (from, _, dest, _) -> filter r from dest
         | SpawnRelation (from, _, _) -> filter r from (annotate "") )
       relations

and is_event_present_on_relation id relation =
  match relation.data with
  | ControlRelation (from, _, dest, _) ->
      from.data = id.data || dest.data = id.data
  | SpawnRelation (from, _, _) -> from.data = id.data

(* =============================================================================
   Aux functions
   ============================================================================= *)

and append_subprograms subprograms =
  let events, template_insts, relations, annotations =
    List.fold_left
      (fun (events, template_insts, relations, annotations)
           (events', template_insts', relations', annotations') ->
        ( List.append events events'
        , List.append template_insts template_insts'
        , List.append relations relations'
        , List.append annotations annotations' ) )
      ([], [], [], []) subprograms
  in
  return (events, template_insts, relations, annotations)

and event_as_expr event = annotate ~loc:event.loc (EventRef (ref event))

(* =============================================================================
   Sorting functions
   ============================================================================= *)

and sort_events program =
  let events = program.events |> List.sort compare in
  return {program with events}

and sort_relations program =
  let relations = program.relations |> List.sort compare in
  return {program with relations}

and sort_program program =
  sort_events program >>= fun program -> sort_relations program

(* =============================================================================
   Alpha-renaming functions
   ============================================================================= *)

let _ = Random.self_init ()

let rec count = ref 0

and counter () =
  let res = !count in
  count := !count + 1 ;
  string_of_int res

and nanoid ?(length = 12) () =
  let chars =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  in
  let chars_len = String.length chars in
  let random_char () = String.get chars (Random.int chars_len) in
  String.init length (fun _ -> random_char ())

and fresh ?(alpha_rename = counter) name =
  Printf.sprintf "%s_%s" name (alpha_rename ())

and fresh_event ?(alpha_rename = counter) event =
  let {info= id, _; _} = event.data in
  set_info ~id:(fresh ~alpha_rename id.data) event

and fresh_event_ids ?(exclude = []) ?(alpha_rename = counter)
    (events, insts, relations, annots) =
  let open Printing in
  let id_env = empty_env in
  fold_left
    (fun id_env event ->
      let id, _ = event.data.info in
      let fresh_id = fresh ~alpha_rename id.data in
      return (bind id.data fresh_id id_env) )
    id_env events
  >>= fun id_env ->
  (* Debug Id Env: *)
  Logger.debug (Printf.sprintf "Id Env: %s" (string_of_env (fun x -> x) id_env)) ;
  (* Exclude all unwanted events *)
  let module StringSet = Set.Make (String) in
  let already_alpha_renamed, _ =
    List.partition_map
      (fun e ->
        let id, _ = e.data.info in
        if String.contains id.data '_' then Left id.data else Right e )
      events
  in
  let exclude_set =
    StringSet.of_list (List.append exclude already_alpha_renamed)
  in
  let event_ids =
    List.map
      (fun e ->
        let id, _ = e.data.info in
        id.data )
      events
  in
  let event_ids_set = StringSet.of_list event_ids in
  let included = StringSet.diff event_ids_set exclude_set in
  (* Alpha rename all the events *)
  map
    (fun event ->
      let id, _ = event.data.info in
      match (find_flat id.data id_env, StringSet.mem id.data included) with
      | Some fresh_id, true -> set_info ~id:fresh_id event
      | _ -> return event )
    events
  >>= fun events ->
  (* Alpha rename all the instances *)
  map
    (fun inst ->
      let {args; _} = inst.data in
      map
        (fun (arg_id, arg) ->
          match arg.data with
          | Identifier id -> (
            match
              (find_flat id.data id_env, StringSet.mem id.data included)
            with
            | Some fresh_id, true ->
                return
                  (arg_id, {arg with data= Identifier {id with data= fresh_id}})
            | _ -> return (arg_id, arg) )
          | _ -> return (arg_id, arg) )
        args
      >>= fun args -> return {inst with data= {inst.data with args}} )
    insts
  >>= fun insts ->
  (* Alpha rename all the relations *)
  map
    (fun relation ->
      (* Debug: relations  *)
      let replace_id id =
        match (find_flat id.data id_env, StringSet.mem id.data included) with
        | Some fresh_id, true -> return {id with data= fresh_id}
        | _ -> return id
      in
      Logger.debug
        (Printf.sprintf "Replacing relation %s"
           (Plain.unparse_relations [relation]) ) ;
      match relation.data with
      | SpawnRelation (from_id, guard, subprogram) ->
          replace_id from_id
          >>= fun from_id ->
          return {relation with data= SpawnRelation (from_id, guard, subprogram)}
      | ControlRelation (from_id, guard, dest_id, op) ->
          replace_id from_id
          >>= fun from_id ->
          replace_id dest_id
          >>= fun dest_id ->
          return
            {relation with data= ControlRelation (from_id, guard, dest_id, op)}
      )
    relations
  >>= fun relations ->
  (* Alpha rename all the annotations *)
  map
    (fun annot ->
      match annot with
      | IfElse {condition; then_branch; else_branch} ->
          let* then_branch =
            fresh_event_ids ~alpha_rename ~exclude then_branch
          in
          let* else_branch =
            match else_branch with
            | Some else_branch ->
                fresh_event_ids ~alpha_rename else_branch >>| fun x -> Some x
            | None -> return None
          in
          return (IfElse {condition; then_branch; else_branch})
      | Foreach (id, expr, body) ->
          let* body = fresh_event_ids ~alpha_rename ~exclude body in
          return (Foreach (id, expr, body)) )
    annots
  >>= fun annots -> return (events, insts, relations, annots)

(* =============================================================================
   Binding functions
   ============================================================================= *)

and bind_events ~f events env =
  fold_left
    (fun env event ->
      let id, _ = event.data.info in
      return (bind id.data (f event) env) )
    env events

(* =============================================================================
   Aux functions
   ============================================================================= *)

and update_ids id_mapping (events, insts, relations, annotations) =
  (* Aux functions *)
  let update id =
    match List.assoc_opt id.data id_mapping with
    | Some new_id -> return new_id
    | None -> return id
  in
  let update_expr expr =
    match expr.data with
    | Identifier id -> (
      match List.assoc_opt id.data id_mapping with
      | Some new_id -> return {expr with data= Identifier new_id}
      | None -> return expr )
    | _ -> return expr
  in
  (* Update each component of the subprogram *)
  let update_events events =
    map
      (fun event ->
        let {info= id, label; _} = event.data in
        update id
        >>= fun new_id ->
        return {event with data= {event.data with info= (new_id, label)}} )
      events
  and update_insts insts =
    let update_args args =
      map
        (fun (arg_id, arg) ->
          Logger.debug
            (Printf.sprintf "Updating arg %s" (Plain.unparse_expr arg)) ;
          update_expr arg >>= fun arg -> return (arg_id, arg) )
        args
    in
    map
      (fun inst ->
        let {args; _} = inst.data in
        update_args args
        >>= fun args -> return {inst with data= {inst.data with args}} )
      insts
  and update_relations relations =
    map
      (fun relation ->
        match relation.data with
        | ControlRelation (from, guard, dest, op) ->
            let* from = update from in
            let* dest = update dest in
            return {relation with data= ControlRelation (from, guard, dest, op)}
        | SpawnRelation (from, guard, subprogram) ->
            let* from = update from in
            let* subprogram = update_ids id_mapping subprogram in
            return {relation with data= SpawnRelation (from, guard, subprogram)}
        )
      relations
  and update_annotations annots =
    let update_annotation annot =
      match annot with
      | IfElse {condition; then_branch; else_branch} ->
          let* condition = update_expr condition in
          let* then_branch = update_ids id_mapping then_branch in
          let* else_branch =
            if Option.is_some else_branch then
              let* else_branch =
                update_ids id_mapping (Option.get else_branch)
              in
              return (Some else_branch)
            else return None
          in
          return (IfElse {condition; then_branch; else_branch})
      | Foreach (id, expr, body) ->
          let* expr = update_expr expr in
          let* body = update_ids id_mapping body in
          return (Foreach (id, expr, body))
    in
    map update_annotation annots
  in
  let* events = update_events events in
  let* insts = update_insts insts in
  let* relations = update_relations relations in
  let* annotations = update_annotations annotations in
  return (events, insts, relations, annotations)

(* =============================================================================
   Aux functions
   ============================================================================= *)

and eval_bool ?(eval = eval_expr) expr env =
  eval expr env
  >>= fun value ->
  match value.data with BoolLit b -> return b | _ -> invalid_expr expr
