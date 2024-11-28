open OUnit2
open Misc
open Monads.ResultMonad
open Templating

(* open Syntax
   open Api
   open Program_helper *)
open Typechecking

let test_suite =
  let open Common in
  let rec test_0 path test_ctxt =
    let result =
      build_state (path "0.tdcr") test_ctxt
      >>= fun (program, event_env, _expr_env) -> typecheck ~event_env program
    in
    result |> expecting_ok
  and test_1 path test_ctxt =
    let result =
      build_state (path "1.tdcr") test_ctxt
      >>= fun (program, event_env, _expr_env) -> typecheck ~event_env program
    in
    result |> expecting_ok
  and test_2 path test_ctxt =
    let result =
      build_state (path "2.tdcr") test_ctxt
      >>= fun (program, event_env, _expr_env) -> typecheck ~event_env program
    in
    result |> expecting_ok
  and test_3 path test_ctxt =
    let result =
      build_state (path "3.tdcr") test_ctxt
      >>= fun (program, event_env, _expr_env) -> typecheck ~event_env program
    in
    result |> expecting_error
  and test_4 path test_ctxt =
    let result =
      build_state (path "4.tdcr") test_ctxt
      >>= fun (program, event_env, _expr_env) -> typecheck ~event_env program
    in
    result |> expecting_error
  and test_5 path test_ctxt =
    let result =
      build_state (path "5.tdcr") test_ctxt
      >>= fun (program, event_env, _expr_env) -> typecheck ~event_env program
    in
    result |> expecting_ok
  and fullpath file = "test/files/typechecker/" ^ file
  and make_test (name, test_fun) =
    name >:: fun test_ctxt -> test_fun fullpath test_ctxt |> ignore
  in
  "typechecking"
  >::: [ make_test ("test_0", test_0)
       ; make_test ("test_1", test_1)
       ; make_test ("test_2", test_2)
       ; make_test ("test_3", test_3)
       ; make_test ("test_4", test_4)
       ; make_test ("test_5", test_5) ]

let _ = run_test_tt_main test_suite
