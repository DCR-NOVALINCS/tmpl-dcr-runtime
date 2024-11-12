open Syntax

(* open Errors *)
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

(* =============================================================================
   Getters
   ============================================================================= *)

(** [get_event ?filter program] returns the event with the identifier [id] in
    the [program] *)
let get_event ?(filter = fun _ -> true) program =
  let events = program.events in
  List.find_opt filter events

let has_event ?(filter = fun _ -> true) program =
  Option.is_some @@ get_event ~filter program

let same_id id e =
  let id', _ = e.data.info in
  id'.data |> String.split_on_char '_' |> List.hd = id

let get_relation ?(filter = fun _ -> true) id program =
  let relations = program.relations in
  List.find_opt
    (fun r ->
      match r.data with
      | ControlRelation (from, _, dest, _, _) ->
          (from.data = id || dest.data = id) && filter r
      | SpawnRelation (from, _, _, _) -> from.data = id && filter r )
    relations

let is_spawn r = match r.data with SpawnRelation _ -> true | _ -> false

let is_ctrl op r =
  match r.data with ControlRelation (_, _, _, op', _) -> op = op' | _ -> false

let has_relation ?(filter = fun _ -> true) id program =
  Option.is_some @@ get_relation ~filter id program
