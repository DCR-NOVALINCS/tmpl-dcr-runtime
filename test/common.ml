open OUnit2
open Misc.Env
(* open Utils *)

(** Setups test context to contain information about the program, the node environment and lattive environment *)
let setup program _test_ctxt = 
  (program, empty_env, empty_env)

(** Teardown function for the test context *)
let teardown _state _test_ctxt = ()

(** Builds the context of the tests, given a filepath of a reda program*)
let build_state program test_ctxt = bracket (setup program) teardown test_ctxt
