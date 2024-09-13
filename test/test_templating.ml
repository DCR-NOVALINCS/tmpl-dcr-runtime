open OUnit2
open Templating.Battery_tests
open Templating.Syntax


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
    "test3" >:: 
    (fun test_ctxt -> 
      let (program, _, _) = build_state _test3 test_ctxt in
      assert_bool "Event a' not found" (has_event "a'" program);
      assert_bool "Event a' don't have any relations" (has_relation "a'" program);
      );
    
    "test4" >:: 
    (fun _ -> assert_equal 1 1);
  ]

let _ = run_test_tt_main test_suite