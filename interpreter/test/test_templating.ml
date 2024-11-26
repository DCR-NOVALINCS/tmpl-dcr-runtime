open OUnit2
open Misc
open Monads.ResultMonad
open Templating
open Syntax
open Api
open Program_helper
(* open Templating.Battery_tests *)
(* open Templating.Errors *)
(* open Templating.Unparser *)

let test_suite =
  let open Common in
  let test_1 path =
    [ ( "test_1"
      >:: fun test_ctxt ->
      build_state (path "1.tdcr") test_ctxt
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
  "templating" >::: ["test_1" >::: test_1 (fun x -> "test/files/simple/" ^ x)]

let _ = run_test_tt_main test_suite
