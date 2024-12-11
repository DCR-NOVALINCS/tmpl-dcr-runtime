(* open OUnit2 *)
open Alcotest
open Misc
open Monads.ResultMonad
open Templating
open Api
open Errors
open Program_helper

let tests_root = "../../../"

let fullpath file = Printf.sprintf "%s%s" tests_root file

let build_program filename =
  parse_program_from_file filename
  >>= fun program ->
  preprocess_program program
  >>= fun (event_env, expr_env, program) ->
  (* TODO: *)
  return (program, event_env, expr_env)

let make_test name filename f expecting =
  let test f _ =
    let result =
      build_program (fullpath filename)
      >>= fun (program, event_env, expr_env) -> f program (event_env, expr_env)
    in
    expecting result
  in
  test_case name `Slow (test f)

let check_bool msg b = Alcotest.(check bool) msg b

let check_true msg b = check_bool msg true b

let check_false msg b = check_bool msg false b

let check_string msg s1 s2 = Alcotest.(check string) msg s1 s2

let check_int msg i1 i2 = Alcotest.(check int) msg i1 i2

let check_list msg l1 l2 = Alcotest.(check (list string)) msg l1 l2

let expecting_ok = function
  | Error e ->
      let msg = pretty_string_errors e in
      Alcotest.fail msg
  | _ -> ()

let expecting_error = function
  | Ok _ -> Alcotest.fail "Expecting error"
  | _ -> ()
