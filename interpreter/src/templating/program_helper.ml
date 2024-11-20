open Syntax
open Evaluation
open Misc.Monads.ResultMonad

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
        if id = id' then event else e )
      program.events
  in
  {program with events}

and update_event_value event expr_env =
  let {marking; io; _} = event.data in
  ( match io.data with
  | Input ty ->
      eval_expr !(marking.data.value) expr_env
      >>= fun value ->
      return (annotate ~loc:io.loc ~ty:!(io.ty) (Input ty), value)
  | Output expr ->
      eval_expr expr expr_env
      >>= fun value ->
      return (annotate ~loc:io.loc ~ty:!(io.ty) (Output value), value) )
  >>= fun (io, value) ->
  set_marking ~marking:(mk_marking ~value:value.data ()) event
  >>= fun event -> return {event with data= {event.data with io}}

and set_marking ?(marking = mk_marking ()) event =
  let {marking= prev_marking; _} = event.data in
  return
    { event with
      data= {event.data with marking= {prev_marking with data= marking}} }

and change_info_event ~new_id ~new_label event =
  let id, label = event.data.info in
  { event with
    data=
      { event.data with
        info= ({id with data= new_id}, {label with data= new_label}) } }

(* =============================================================================
   Updating relation functions
   ============================================================================= *)

and change_relation old_id new_id relation =
  match relation.data with
  | ControlRelation (from, guard, dest, t, annot) ->
      let new_from = if from.data = old_id.data then new_id else from in
      let new_dest = if dest.data = old_id.data then new_id else dest in
      {relation with data= ControlRelation (new_from, guard, new_dest, t, annot)}
  | SpawnRelation (from, guard, subprogram, annot) ->
      let new_from = if from.data = old_id.data then new_id else from in
      {relation with data= SpawnRelation (new_from, guard, subprogram, annot)}

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

and same_id id e =
  let id', _ = e.data.info in
  id'.data |> String.split_on_char '_' |> List.hd = id

and get_relation ?(filter = fun _ -> true) id program =
  let relations = program.relations in
  List.find_opt
    (fun r ->
      match r.data with
      | ControlRelation (from, _, dest, _, _) ->
          (from.data = id || dest.data = id) && filter r
      | SpawnRelation (from, _, _, _) -> from.data = id && filter r )
    relations

and is_spawn r = match r.data with SpawnRelation _ -> true | _ -> false

and is_ctrl op r =
  match r.data with ControlRelation (_, _, _, op', _) -> op = op' | _ -> false

and has_relation ?(filter = fun _ -> true) id program =
  Option.is_some @@ get_relation ~filter id program

and is_event_present_on_relation id relation =
  match relation.data with
  | ControlRelation (from, _, dest, _, _) ->
      from.data = id.data || dest.data = id.data
  | SpawnRelation (from, _, _, _) -> from.data = id.data

(* =============================================================================
   Aux functions
   ============================================================================= *)

and event_as_expr event =
  (* let {marking; _} = event.data in *)
  let {marking; _} = event.data in
  (* let _, label = info in *)
  annotate ~loc:event.loc ~ty:!(marking.ty)
    (Record [(annotate ~ty:!(marking.ty) "value", !(marking.data.value))])

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
  let id, label = event.data.info in
  change_info_event ~new_id:(fresh id.data) ~new_label:label.data event

and fresh_event_ids events relations exports_mapping =
  map
    (fun event ->
      let id, label = event.data.info in
      let export_id =
        match List.assoc_opt id.data exports_mapping with
        | None -> id
        | Some new_id -> new_id
      in
      let fresh_id = annotate ~loc:id.loc ~ty:!(id.ty) (fresh export_id.data) in
      let fresh_event =
        change_info_event ~new_id:fresh_id.data ~new_label:label.data event
      in
      return (id, fresh_id, fresh_event) )
    events
  >>= fun events_mapping ->
  map
    (fun relation ->
      fold_left
        (fun relation (old_id, new_id, _) ->
          if is_event_present_on_relation old_id relation then
            return (change_relation old_id new_id relation)
          else return relation )
        relation events_mapping )
    relations
  >>= fun fresh_relations ->
  let fresh_events = List.map (fun (_, _, e) -> e) events_mapping in
  return (fresh_events, fresh_relations)
