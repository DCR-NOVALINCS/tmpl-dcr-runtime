open OUnit2
open Misc.Monads.ResultMonad
open Templating.Syntax
open Templating.Api
(* open Templating.Battery_tests *)
(* open Templating.Errors *)
(* open Templating.Unparser *)

(** [get_event id program] returns the event with id [id] in the program
    [program] *)
let get_event ?(filter = fun _ -> true) program =
  let events = program.events in
  List.find_opt filter events

(** [has_event id program] returns true if the event with id [id] is found in
    the program [program] *)
let has_event ?(filter = fun _ -> true) program =
  Option.is_some @@ get_event ~filter program

let same_id id e =
  let id', _ = e.data.info in
  id'.data |> String.split_on_char '_' |> List.hd = id

(** [get_relation id program] returns the relation with id [id] in the program
    [program] *)
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

(** [has_relation id program] returns true if the relation with id [id] is found
    in the program [program] *)
let has_relation ?(filter = fun _ -> true) id program =
  Option.is_some @@ get_relation ~filter id program

let test_suite =
  let open Common in
  let simple_tests path =
    [ ( "test_1"
      >:: fun test_ctxt ->
      build_state (path "/1.tdcr") test_ctxt
      >>= (fun (program, _, _) ->
            assert_bool "Event a not found"
              (has_event ~filter:(same_id "a") program) ;
            assert_bool "Event a doesn't have any relations"
              (has_relation "a" program) ;
            execute ~event_id:"a" ~expr:(IntLit 1) program
            >>= fun program ->
            assert_bool "Expected 3 events" (List.length program.events = 3) ;
            assert_bool "Expected 2 relations"
              (List.length program.relations = 2) ;
            assert_bool "Not extecting instantiations to be done."
              (List.length program.template_insts = 0) ;
            assert_bool "Event a not found"
              (has_event ~filter:(same_id "a") program) ;
            assert_bool "Event b not found"
              (has_event ~filter:(same_id "b") program) ;
            assert_bool "Event a doesn't have any relations"
              (has_relation "a" program) ;
            assert_bool "Expecting spawn relation (-->>) from a"
              (has_relation ~filter:is_spawn "a" program) ;
            assert_bool
              "Not expecting condition relation (-->*) from 'e' to instantiated 'b'"
              (not @@ has_relation ~filter:(is_ctrl Condition) "e" program) ;
            assert_bool
              "Expecting condition relation (-->*) from a to instantiated 'b'"
              (has_relation ~filter:(is_ctrl Condition) "a" program) ;
            execute ~event_id:"a" ~expr:(IntLit 2) program
            >>= fun program ->
            assert_bool "Expected 5 events" (List.length program.events = 5) ;
            assert_bool "Expected 3 relations"
              (List.length program.relations = 3) ;
            assert_bool "Expected 2 condition relations"
              ( List.length @@ List.filter (is_ctrl Condition) program.relations
              = 2 ) ;
            Ok program )
      |> expecting_ok |> ignore ) ]
  in
  let exported_events_tests path =
    [ ( "test_1"
      >:: fun test_ctxt ->
      build_state (path "/1.tdcr") test_ctxt |> expecting_ok |> ignore )
    ; ( "test_2"
      >:: fun test_ctxt ->
      build_state (path "/2.tdcr") test_ctxt |> expecting_ok |> ignore )
    ; ( "test_3"
      >:: fun test_ctxt ->
      build_state (path "/3.tdcr") test_ctxt |> expecting_ok |> ignore ) ]
  in
  let annotations_tests path =
    [ ( "test_4"
      >:: fun test_ctxt ->
      build_state (path "/4.tdcr") test_ctxt |> expecting_ok |> ignore )
    ; ( "test_6"
      >:: fun test_ctxt ->
      build_state (path "/6.tdcr") test_ctxt |> expecting_ok |> ignore ) ]
  in
  let error_tests path =
    [ ( "test_4"
      >:: fun test_ctxt ->
      build_state (path "/7.tdcr") test_ctxt |> expecting_ok |> ignore ) ]
  in
  "templating"
  >::: [ "simple" >::: simple_tests (fun file -> "test/files/simple" ^ file)
       ; "exported-events"
         >::: exported_events_tests (fun file ->
                  "test/files/exported-events" ^ file )
       ; "annotations"
         >::: annotations_tests (fun file -> "test/files/annotations" ^ file)
       ; "error" >::: error_tests (fun file -> "test/files/error" ^ file) ]

let _ = run_test_tt_main test_suite
