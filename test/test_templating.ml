open OUnit2
open Templating.Battery_tests
open Templating.Syntax
open Templating.Runtime
open Misc.Monads

(**
  [get_event id program]
  returns the event with id [id] in the program [program]
*)
let get_event id program = 
  let events = program.events in
  List.find_opt (fun e -> 
    let (id', _) = e.info in id' = id) events

(**
  [has_event id program]
  returns true if the event with id [id] is found in the program [program]
*)
let has_event id program = 
  Option.is_some @@ get_event id program

(**
  [get_relation id program]
  returns the relation with id [id] in the program [program]
*)
let get_relation id program = 
  let relations = program.relations in
  List.find_opt (fun r -> 
    match r with
    | ControlRelation (from, _, dest, _) -> from = id || dest = id
    | SpawnRelation (from, _, _) -> from = id) relations

(**
  [has_relation id program]
  returns true if the relation with id [id] is found in the program [program]
*)
let has_relation id program = 
  Option.is_some @@ get_relation id program

let test_suite = 
  let open Common in
  "test_templating" >::: [
    "test0" >:: 
    (fun test_ctxt -> 
      (Ok (build_state _test0 test_ctxt) 
      >>= fun (program, event_env, expr_env) ->
      assert_bool "Event a' not found" (has_event "a'" program);
      assert_bool "Event a' don't have any relations" (has_relation "a'" program);
      execute ~event_env ~expr_env ~event_id:"a'" program
      >>= fun program -> 
      assert_bool "Event a' not found" (has_event "a'" program);
      assert_bool "Event a' don't have any relations" (has_relation "a'" program);
      Ok program)
      |> function
      | Ok _ -> ()
      | Error e -> assert_failure e
      );
    
    "test1" >:: 
    (fun test_ctxt -> 
      Ok (build_state _test1 test_ctxt) 
      >>| fun (_program, _, _) ->
      assert_bool "TODO TEST" true;
      |> Result.get_ok
      );

    "test2" >::
    (fun test_ctxt -> 
      Ok (build_state _test2 test_ctxt)
      >>| fun (_program, _, _) ->
      assert_bool "TODO TEST" true;
      |> Result.get_ok
      );

    "test3" >::
    (fun test_ctxt -> 
      Ok (build_state _test3 test_ctxt)
      >>| fun (_program, _, _) ->
      assert_bool "TODO TEST" true;
      |> Result.get_ok
      );

    "test4" >::
    (fun test_ctxt ->
      Ok (build_state _test4 test_ctxt)
      >>| fun (_program, _, _) ->
      assert_bool "TODO TEST" true;
      |> Result.get_ok
      );

    "test5" >::
    (fun test_ctxt ->
      Ok (build_state _test5 test_ctxt)
      >>| fun (_program, _, _) ->
      assert_bool "TODO TEST" true;
      |> Result.get_ok
      );

    "test6" >::
    (fun test_ctxt ->
      Ok (build_state _test6 test_ctxt)
      >>| fun (_program, _, _) ->
      assert_bool "TODO TEST" true;
      |> Result.get_ok
      );

    "test7" >::
    (fun test_ctxt ->
      Ok (build_state _test7 test_ctxt)
      >>| fun (_program, _, _) ->
      assert_bool "TODO TEST" true;
      |> Result.get_ok
      );
  ]

let _ = run_test_tt_main test_suite