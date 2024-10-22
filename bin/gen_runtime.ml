open Js_of_ocaml
open Templating.Api
open Templating.Syntax
(* open Misc.Monads *)

(*
==============================
  Runtime for the web editor

  Available functions:
  - view
  - execute (not implemented)
  - 

*)

let active_program = ref empty_program

let add_event ~id ~label = 
  let info = (annotate id, annotate label) in
  let io = annotate @@ Input (annotate UnitTy) in
  active_program := { !active_program with events = mk_event info io :: !active_program.events }

let view =
  fun () ->
  view !active_program
  |> function
  | Error _ -> Js.string ("Error on view")
  | Ok unparsed_program -> Js.string unparsed_program

let runtime () = 
  Js.export "view" view;
  Js.export "add_event" (fun (id: Js.js_string Js.t) (label: Js.js_string Js.t) -> add_event ~id:(Js.to_string id) ~label:(Js.to_string label))

let _ = runtime ()