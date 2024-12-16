open Syntax
open Evaluation
open Errors
open Misc
open Monads.ResultMonad
open Env
(* open Printing *)

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
  return
    { event with
      data= {event.data with marking= {marking with data= new_marking}} }

and update_event_io ?(eval = eval_expr) event expr_env =
  let {marking; io; _} = event.data in
  match io.data with
  | Input _ ->
      eval !(marking.data.value) expr_env
      >>= fun value -> set_marking ~value event
  | Output expr ->
      eval expr expr_env
      >>= fun value ->
      set_marking ~value event
      >>= fun event ->
      return
        {event with data= {event.data with io= {io with data= Output value}}}

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

(* =============================================================================
   Updating relation functions
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
    (events, template_insts, relations, _) =
  preprocess_program ~expr_env ~event_env
    {empty_program with events; template_insts; relations}
  >>= fun (event_env, expr_env, program) ->
  return
    ( event_env
    , expr_env
    , (program.events, program.template_insts, program.relations, []) )

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
  | None -> id_not_found id

and find_all_events ?(filter = fun _ -> true) program =
  let events = program.events in
  return @@ List.filter filter events

and same_id id e =
  let id', _ = e.data.info in
  id'.data |> String.split_on_char '_' |> List.hd = id

and is_executed id program =
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
      included.data

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
  fold_right
    (fun (events, template_insts, relations, annotations)
         (events', template_insts', relations', annotations') ->
      return
      @@ mk_subprogram
           ~events:(List.append events' events)
           ~template_insts:(List.append template_insts' template_insts)
           ~relations:(List.append relations' relations)
           ~annotations:(List.append annotations' annotations)
           () )
    empty_subprogram subprograms

and event_as_expr event =
  (* let {marking; _} = event.data in *)
  (* let {marking; io; _} = event.data in
     let value =
       match (marking.data, io.data) with
       | _, Output expr -> expr
       | {value; _}, Input _ -> !value
     in *)
  (* let _, label = info in *)
  (* annotate ~loc:event.loc ~ty:!(marking.ty)
     (Record [(annotate ~ty:!(marking.ty) "value", value)]) *)
  annotate ~loc:event.loc (EventRef (ref event))

and event_as_ty event =
  let {io; _} = event.data in
  let ty =
    match io.data with
    | Input ty -> ty.data
    | Output expr -> (
      match !(expr.ty) with None -> failwith "Type not found" | Some ty -> ty )
  in
  RecordTy [(annotate "value", annotate ty)]

(* =============================================================================
   Alpha-renaming functions
   ============================================================================= *)

and r = Random.self_init ()

and count = ref 0

and counter _ =
  let res = !count in
  count := !count + 1 ;
  string_of_int res

and nanoid ?(length = 12) _ =
  let chars =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  in
  let chars_len = String.length chars in
  let random_char () = String.get chars (Random.int chars_len) in
  String.init length (fun _ -> random_char ())

and fresh ?(id_fn = counter) name = Printf.sprintf "%s_%s" name (id_fn ())

and fresh_event event =
  let id, _ = event.data.info in
  set_info ~id:(fresh id.data) event

and fresh_event_ids ?(exclude = []) events relations =
  (* map
       (fun event ->
         let id, _ = event.data.info in
         let fresh_id = {id with data= fresh id.data} in
         let* fresh_event =
           (* change_info_event ~new_id:fresh_id.data ~new_label:label.data event *)
           set_info ~id:fresh_id.data event
         in
         return (id, fresh_id, fresh_event) )
       events
     >>= fun events_mapping ->
     map
       (fun relation ->
         (* Debug: relations  *)
         let open Printing in
         Logger.debug
           (Printf.sprintf "Replacing relation %s"
              (Unparser.PlainUnparser.unparse_relations [relation]) ) ;
         fold_left
           (fun relation (old_id, new_id, _) ->
             if is_event_present_on_relation old_id relation then
               return (change_relation old_id new_id relation)
             else return relation )
           relation events_mapping )
       relations
     >>= fun fresh_relations ->
     let fresh_events = List.map (fun (_, _, e) -> e) events_mapping in
     return (fresh_events, fresh_relations) *)
  let open Printing in
  let id_env = empty_env in
  fold_left
    (fun id_env event ->
      let id, _ = event.data.info in
      let fresh_id = fresh id.data in
      return (bind id.data fresh_id id_env) )
    id_env events
  >>= fun id_env ->
  (* Debug Id Env: *)
  Logger.debug (Printf.sprintf "Id Env: %s" (string_of_env (fun x -> x) id_env)) ;
  (* Exclude all unwanted events *)
  let module StringSet = Set.Make (String) in
  let exclude = StringSet.of_list exclude in
  let event_ids =
    List.map
      (fun e ->
        let id, _ = e.data.info in
        id.data )
      events
  in
  let event_ids_set = StringSet.of_list event_ids in
  let included = StringSet.diff event_ids_set exclude in
  (* Alpha rename all the events *)
  map
    (fun event ->
      let id, _ = event.data.info in
      match (find_flat id.data id_env, StringSet.mem id.data included) with
      | Some fresh_id, true -> set_info ~id:fresh_id event
      | _ -> return event )
    events
  >>= fun events ->
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
           (Unparser.PlainUnparser.unparse_relations [relation]) ) ;
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
  >>= fun relations -> return (events, relations)

(* =============================================================================
   Binding functions
   ============================================================================= *)

and bind_events ~f events env =
  fold_left
    (fun env event ->
      let id, _ = event.data.info in
      return (bind id.data (f event) env) )
    env events
