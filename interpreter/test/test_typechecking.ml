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
  let test_0 path =
    [ ( "test_0"
      >:: fun test_ctxt ->
      let result =
        build_state (path "0.tdcr") test_ctxt
        >>= fun (program, event_env, _expr_env) -> typecheck ~event_env program
      in
      result |> expecting_ok |> ignore ) ]
  and test_1 path =
    [ ( "test_1"
      >:: fun test_ctxt ->
      let result =
        build_state (path "1.tdcr") test_ctxt
        >>= fun (program, event_env, _expr_env) -> typecheck ~event_env program
      in
      result |> expecting_ok |> ignore ) ]
  in
  "templating"
  >::: [ "test_0" >::: test_0 (fun x -> "test/files/typechecker/" ^ x)
       ; "test_1" >::: test_1 (fun x -> "test/files/typechecker/" ^ x) ]

let _ = run_test_tt_main test_suite
