(* open OUnit2 *)
open Alcotest
open Misc
open Monads.ResultMonad
open Templating
open Api
open Errors

(* open Syntax
   open Lex_and_parse *)
open Program_helper

(* let root_path = "../../../"

   let parse filename =
     try
       let filepath = Printf.sprintf "%s%s" root_path filename in
       let file = open_in filepath in
       let lexbuf = Lexing.from_channel file in
       parse_program ~filename lexbuf >>| fun program -> close_in file ; program
     with
     | Sys_error _msg -> file_not_exists filename
     | _ -> should_not_happen "Unknown error"

   let setup filename _test_ctxt =
     (* Logger.disable () ; *)
     parse filename
     >>= fun program ->
     preprocess_program program
     >>= fun (event_env, expr_env, program) ->
     (* instantiate ~expr_env program *)
     (* >>= fun (program, expr_env) ->  *)
     return (program, event_env, expr_env)

   let teardown _state _test_ctxt = ()

   let build_state filename test_ctxt = bracket (setup filename) teardown test_ctxt

   let expecting_ok = function
     | Error e -> pretty_string_errors e |> assert_failure
     | _ as result -> result

   let expecting_error ?(msg = "") = function
     | Ok _ -> assert_failure @@ Printf.sprintf "Expecting error: %s" msg
     | _ as result -> result *)

let tests_root = "../../../"

let fullpath file = Printf.sprintf "%s%s" tests_root file

let build_program filename =
  parse_program_from_file filename
  >>= fun program ->
  preprocess_program program
  >>= fun (event_env, expr_env, program) -> return (program, event_env, expr_env)

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

let expecting_ok _ = function
  | Error e ->
      let msg = pretty_string_errors e in
      Alcotest.fail msg
  | _ -> ()

let expecting_error _ = function
  | Ok _ -> Alcotest.fail "Expecting error"
  | _ -> ()
