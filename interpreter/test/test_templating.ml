(* open OUnit2 *)
open Alcotest
open Misc
open Monads.ResultMonad
open Templating
open Syntax
open Api
open Program_helper
open Instantiation
open Common
(* open Templating.Battery_tests *)
(* open Templating.Errors *)
(* open Templating.Unparser *)

(* let test_suite =
     let open Common in
     let rec test_1 path test_ctxt =
       let result =
         build_state (path "1.tdcr") test_ctxt
         >>= fun (program, event_env, expr_env) ->
         instantiate ~expr_env ~event_env program
         >>= fun (program, event_env, expr_env) ->
         check_true "Event a not found" (has_event ~filter:(same_id "a") program) ;
         check_true "Event a doesn't have any relations"
           (has_relation "a" program) ;
         execute ~event_id:"a" ~expr:(IntLit 1) ~expr_env ~event_env program
         >>= fun (program, event_env, expr_env) ->
         check_true "Expected 3 events" (List.length program.events = 3) ;
         check_true "Expected 2 relations" (List.length program.relations = 2) ;
         check_true "Not expecting instantiations to be done."
           (List.length program.template_insts = 0) ;
         check_true "Event a not found" (has_event ~filter:(same_id "a") program) ;
         check_true "Event b not found" (has_event ~filter:(same_id "b") program) ;
         check_true "Event a doesn't have any relations"
           (has_relation "a" program) ;
         check_true "Expecting spawn relation (-->>) from a"
           (has_relation ~filter:is_spawn "a" program) ;
         check_true
           "Not expecting condition relation (-->*) from 'e' to instantiated 'b'"
           (not @@ has_relation ~filter:(is_ctrl Condition) "e" program) ;
         check_true
           "Expecting condition relation (-->*) from a to instantiated 'b'"
           (has_relation ~filter:(is_ctrl Condition) "a" program) ;
         execute ~event_id:"a" ~expr:(IntLit 2) ~expr_env ~event_env program
         >>= fun program ->
         (* check_true "Expected 5 events" (List.length program.events = 5) ;
            check_true "Expected 3 relations" (List.length program.relations = 3) ;
            check_true "Expected 2 condition relations"
              (List.length @@ List.filter (is_ctrl Condition) program.relations = 2) ; *)
         return program
       in
       result |> expecting_ok
     and fullpath file = "test/files/simple/" ^ file
     and make_test (name, test_fun) =
       name >:: fun test_ctxt -> test_fun fullpath test_ctxt |> ignore
     in
     "templating" >::: [make_test ("test_1", test_1)]

   let _ = run_test_tt_main test_suite *)

let tests_folder = "test/files/simple"

let test_suite =
  [ ( "basic"
    , [ make_test "1.tdcr"
          (Printf.sprintf "%s/%s" tests_folder "1.tdcr")
          (fun program (event_env, expr_env) ->
            instantiate ~expr_env ~event_env program
            >>= fun (program, event_env, expr_env) ->
            check_true "Event a not found"
              (has_event ~filter:(same_id "a") program) ;
            check_true "Event a doesn't have any relations"
              (has_relation "a" program) ;
            execute ~event_id:"a" ~expr:(IntLit 1) ~expr_env ~event_env program
            >>= fun (program, event_env, expr_env) ->
            check_true "Expected 3 events" (List.length program.events = 3) ;
            check_true "Expected 2 relations" (List.length program.relations = 2) ;
            check_true "Not expecting instantiations to be done."
              (List.length program.template_insts = 0) ;
            check_true "Event a not found"
              (has_event ~filter:(same_id "a") program) ;
            check_true "Event b not found"
              (has_event ~filter:(same_id "b") program) ;
            check_true "Event a doesn't have any relations"
              (has_relation "a" program) ;
            check_true "Expecting spawn relation (-->>) from a"
              (has_relation ~filter:is_spawn "a" program) ;
            check_true
              "Not expecting condition relation (-->*) from 'e' to instantiated 'b'"
              (not @@ has_relation ~filter:(is_ctrl Condition) "e" program) ;
            check_true
              "Expecting condition relation (-->*) from a to instantiated 'b'"
              (has_relation ~filter:(is_ctrl Condition) "a" program) ;
            execute ~event_id:"a" ~expr:(IntLit 2) ~expr_env ~event_env program
            >>= fun program -> return program )
          (expecting_ok ()) ] ) ]

let _ = run ~verbose:false "templating" test_suite
