(* open OUnit2 *)
open Alcotest
open Misc
open Monads.ResultMonad
open Templating
open Typechecking
open Common

let fun_to_test program (event_env, expr_env) =
  typecheck ~event_env program >>= fun _ -> return (program, event_env, expr_env)

let tests_folder = "test/files/typechecker"

let file name = Printf.sprintf "%s/%s" tests_folder name

let test_suite =
  [ ( "basic"
    , [ make_test "0.tdcr" (file "0.tdcr") fun_to_test (expecting_ok ())
      ; make_test "1.tdcr" (file "1.tdcr") fun_to_test (expecting_ok ())
      ; make_test "2.tdcr" (file "2.tdcr") fun_to_test (expecting_ok ())
      ; make_test "3.tdcr" (file "3.tdcr") fun_to_test (expecting_error ())
      ; make_test "4.tdcr" (file "4.tdcr") fun_to_test (expecting_error ())
      ; make_test "5.tdcr" (file "5.tdcr") fun_to_test (expecting_ok ())
      ; make_test "6.tdcr" (file "6.tdcr") fun_to_test (expecting_error ())
      ; make_test "7.tdcr" (file "7.tdcr") fun_to_test (expecting_ok ())
      ; make_test "8.tdcr" (file "8.tdcr") fun_to_test (expecting_ok ()) ] ) ]

let _ = run ~and_exit:false ~verbose:false "typechecking" test_suite
