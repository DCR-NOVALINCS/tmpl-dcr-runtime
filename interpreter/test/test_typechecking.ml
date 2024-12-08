open OUnit2
open Misc
open Monads.ResultMonad
open Templating
open Typechecking

let test_suite =
  let open Common in
  (* let open Printing in
     Logger.enable () ; *)
  let rec test_0 path test_ctxt =
    let result =
      build_state (path "0.tdcr") test_ctxt
      >>= fun (program, event_env, _) -> typecheck ~event_env program
    in
    result |> expecting_ok
  and test_1 path test_ctxt =
    let result =
      build_state (path "1.tdcr") test_ctxt
      >>= fun (program, event_env, _) -> typecheck ~event_env program
    in
    result |> expecting_ok
  and test_2 path test_ctxt =
    let result =
      build_state (path "2.tdcr") test_ctxt
      >>= fun (program, event_env, _) -> typecheck ~event_env program
    in
    result |> expecting_ok
  and test_3 path test_ctxt =
    let result =
      build_state (path "3.tdcr") test_ctxt
      >>= fun (program, event_env, _) -> typecheck ~event_env program
    in
    result |> expecting_error
  and test_4 path test_ctxt =
    let result =
      build_state (path "4.tdcr") test_ctxt
      >>= fun (program, event_env, _) -> typecheck ~event_env program
    in
    result |> expecting_error
  and test_5 path test_ctxt =
    let result =
      build_state (path "5.tdcr") test_ctxt
      >>= fun (program, event_env, _) -> typecheck ~event_env program
    in
    result |> expecting_ok
  and test_6 path test_ctxt =
    let result =
      build_state (path "6.tdcr") test_ctxt
      >>= fun (program, event_env, _) -> typecheck ~event_env program
    in
    result |> expecting_error
  and test_7 path test_ctxt =
    let result =
      build_state (path "7.tdcr") test_ctxt
      >>= fun (program, event_env, _) -> typecheck ~event_env program
    in
    result |> expecting_ok
  and test_8 path test_ctxt =
    let result =
      build_state (path "8.tdcr") test_ctxt
      >>= fun (program, event_env, _) ->
      typecheck ~event_env program >>= fun _ -> return ()
    in
    result |> expecting_ok
  and fullpath file = "test/files/typechecker/" ^ file
  and make_test (name, test_fun) =
    name >:: fun test_ctxt -> test_fun fullpath test_ctxt |> ignore
  in
  "typechecking"
  >::: [ make_test ("test.0", test_0)
       ; make_test ("test.1", test_1)
       ; make_test ("test.2", test_2)
       ; make_test ("test.3", test_3)
       ; make_test ("test.4", test_4)
       ; make_test ("test.5", test_5)
       ; make_test ("test.6", test_6)
       ; make_test ("test.7", test_7)
       ; make_test ("test.8", test_8) ]

let _ = run_test_tt_main test_suite
