open Alcotest
open Misc
open Monads.ResultMonad
open Templating
open Syntax
open Api
open Program_helper
open Instantiation
open Runtime
open Typechecking
open Common

(* =============================================================================
   Simple Section
   ============================================================================= *)

let tests_folder = "test/files"

let file name = Printf.sprintf "%s/%s" tests_folder name

let simple_set =
  (* Note: The following tests assume that the programs are typed correctly. *)
  [ make_test "0.tdcr" (file "simple/0.tdcr")
      (fun program (event_env, expr_env) ->
        (* Testing the number of elements before instantiating *)
        check_int "Expected 1 event" 1 (List.length program.events) ;
        check_int "Expected 1 template instantiation" 1
          (List.length program.template_insts) ;
        check_int "Expected 0 relation" 0 (List.length program.relations) ;
        let g_tmpl =
          List.find_opt (fun x -> x.id.data = "g") program.template_decls
        in
        check_true "Expected template g to be found" (Option.is_some g_tmpl) ;
        let g_tmpl = Option.get g_tmpl in
        check_int "Expected 2 parameters" 2 (List.length g_tmpl.params) ;
        let g_events, g_insts, g_relations = g_tmpl.graph in
        check_int "Expected 1 event" 1 (List.length g_events) ;
        check_int "Expected 0 template instantiation" 0 (List.length g_insts) ;
        check_int "Expected 1 relation" 1 (List.length g_relations) ;
        instantiate ~expr_env ~event_env program
        >>= fun (program, event_env, _expr_env) ->
        (* Testing the number of elements after instantiation *)
        check_int "Expected 2 events" 2 (List.length program.events) ;
        check_int "Expected 2 events in the event env" 2
          (List.length (Env.flatten event_env)) ;
        check_int "Expected no template instantiation" 0
          (List.length program.template_insts) ;
        check_int "Expected 1 relation" 1 (List.length program.relations) ;
        (* Check id of instantiated events from template [g] *)
        let b = get_event ~filter:(same_id "b") program in
        check_false "Not expecting to find event b"
          (has_event
             ~filter:(fun e ->
               let id, _ = e.data.info in
               id.data = "b" )
             program ) ;
        check_true "Event b not found" (Option.is_some b) ;
        let b = Option.get b in
        check_true "Expecting to have a relation related to the event b"
          (has_relation ~filter:(is_ctrl Condition) (fst b.data.info).data
             program ) ;
        execute ~event_id:"a" ~expr:Unit ~expr_env ~event_env program
        >>= fun (program, event_env, expr_env) ->
        check_true "Event a should be executed" (is_executed "a" program) ;
        let b = Option.get (get_event ~filter:(same_id "b") program) in
        check_true "Event b should be enabled"
          ( is_enabled b program (event_env, expr_env)
          |> function Ok b -> b | _ -> false ) ;
        return program )
      expecting_ok
  ; make_test "1.tdcr" (file "simple/1.tdcr")
      (fun program (event_env, expr_env) ->
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
        >>= fun program -> return program )
      expecting_ok
  ; make_test "2.tdcr" (file "simple/2.tdcr")
      (fun program (event_env, expr_env) ->
        check_int "Expected 1 event" 1 (List.length program.events) ;
        check_int "Expected 0 template instantiation" 0
          (List.length program.template_insts) ;
        check_int "Expected 2 relations" 2 (List.length program.relations) ;
        let spawn_relations = find_all_relations ~filter:is_spawn "a" program in
        check_int "Expecting two spawn relations from 'a'" 2
          (List.length spawn_relations) ;
        execute ~event_id:"a" ~expr:Unit ~expr_env ~event_env program
        >>= fun (program, _event_env, _expr_env) ->
        check_true "Event a should be executed" (is_executed "a" program) ;
        check_int "Expected 5 events" 5 (List.length program.events) ;
        check_int "Expected 4 relations" 4 (List.length program.relations) ;
        check_int "Not expecting instantiations to be done." 0
          (List.length program.template_insts) ;
        let bs = find_all_events ~filter:(same_id "b") program in
        check_int "Expecting 4 events with sub-id 'b'" 4 (List.length bs) ;
        return program )
      expecting_ok
  ; make_test "3.tdcr" (file "simple/3.tdcr")
      (fun program (event_env, expr_env) ->
        check_int "Expected 1 event" 1 (List.length program.events) ;
        check_int "Expected 0 template instantiation" 0
          (List.length program.template_insts) ;
        check_int "Expected 1 relation" 1 (List.length program.relations) ;
        let spawn_relations = find_all_relations ~filter:is_spawn "c" program in
        check_int "Expecting one spawn relation from 'c'" 1
          (List.length spawn_relations) ;
        let spawn_relation = List.hd spawn_relations in
        let spawn_events, spawn_insts, spawn_relations =
          match spawn_relation.data with
          | SpawnRelation (_, _, graph) -> graph
          | _ -> failwith "Expected spawn relation"
        in
        check_int "Not expecting any events in the spawn relation" 0
          (List.length spawn_events) ;
        check_int "Not expecting any relations in the spawn relation" 0
          (List.length spawn_relations) ;
        check_int "Expecting 2 template instantiations" 2
          (List.length spawn_insts) ;
        execute ~event_id:"c" ~expr:(IntLit 0) ~expr_env ~event_env program
        >>= fun (program, event_env, expr_env) ->
        check_int "Expected 3 events" 3 (List.length program.events) ;
        check_int "Expected 1 relation" 1 (List.length program.relations) ;
        check_int "Not expecting instantiations to be done." 0
          (List.length program.template_insts) ;
        let positives = find_all_events ~filter:(same_id "positive") program in
        check_int "Expecting 1 event with sub-id 'positive'" 1
          (List.length positives) ;
        let negatives = find_all_events ~filter:(same_id "negative") program in
        check_int "Expecting 1 event with sub-id 'negative'" 1
          (List.length negatives) ;
        let all_contain_false_value =
          List.for_all
            (fun o ->
              let {io; _} = o.data in
              match io.data with
              | Output value -> value.data = False
              | _ -> false )
            (List.append positives negatives)
        in
        check_true "Expecting all instantiated 'o' to have value False"
          all_contain_false_value ;
        execute ~event_id:"c" ~expr:(IntLit 1) ~expr_env ~event_env program
        >>= fun (program, _event_env, _expr_env) ->
        check_int "Expected 5 events" 5 (List.length program.events) ;
        check_int "Expected 1 relations" 1 (List.length program.relations) ;
        check_int "Not expecting instantiations to be done." 0
          (List.length program.template_insts) ;
        let positives = find_all_events ~filter:(same_id "positive") program in
        check_int "Expecting 2 events with sub-id 'positive'" 2
          (List.length positives) ;
        let negatives = find_all_events ~filter:(same_id "negative") program in
        check_int "Expecting 2 events with sub-id 'negative'" 2
          (List.length negatives) ;
        let all_positive_true, _ =
          List.partition_map
            (fun o ->
              let {io; _} = o.data in
              match io.data with
              | Output value -> Left (Unparser.PlainUnparser.unparse_expr value)
              | _ -> Right o )
            positives
        in
        let all_negative_false, _ =
          List.partition_map
            (fun o ->
              let {io; _} = o.data in
              match io.data with
              | Output value -> Left (Unparser.PlainUnparser.unparse_expr value)
              | _ -> Right o )
            negatives
        in
        check_list
          "Expecting all instantiated 'positive' to have value True and False, respectively"
          [ Unparser.PlainUnparser.unparse_expr (annotate True)
          ; Unparser.PlainUnparser.unparse_expr (annotate False) ]
          all_positive_true ;
        check_list
          "Expecting all instantiated 'negative' to have both False value"
          [ Unparser.PlainUnparser.unparse_expr (annotate False)
          ; Unparser.PlainUnparser.unparse_expr (annotate False) ]
          all_negative_false ;
        return program )
      expecting_ok ]

(* =============================================================================
   Typechecking Section
   ============================================================================= *)

let fun_to_test program (event_env, expr_env) =
  typecheck ~event_env program >>= fun _ -> return (program, event_env, expr_env)

let typecheck_set =
  [ make_test "0.tdcr" (file "typechecker/0.tdcr") fun_to_test expecting_ok
  ; make_test "1.tdcr" (file "typechecker/1.tdcr") fun_to_test expecting_ok
  ; make_test "2.tdcr" (file "typechecker/2.tdcr") fun_to_test expecting_ok
  ; make_test "3.tdcr" (file "typechecker/3.tdcr") fun_to_test expecting_error
  ; make_test "4.tdcr" (file "typechecker/4.tdcr") fun_to_test expecting_error
    (* ; make_test "5.tdcr" (file "typechecker/5.tdcr") fun_to_test expecting_ok *)
  ; make_test "6.tdcr" (file "typechecker/6.tdcr") fun_to_test expecting_error
    (* ; make_test "7.tdcr" (file "typechecker/7.tdcr") fun_to_test expecting_ok *)
    (* ; make_test "8.tdcr" (file "typechecker/8.tdcr") fun_to_test expecting_ok  *)
  ]

(* =============================================================================
   Export Events Section
   ============================================================================= *)

let export_events_set =
  (* Note: assuming that the following programs are typed correctly *)
  [ make_test "3.tdcr"
      (file "exported-events/3.tdcr")
      (fun program (event_env, expr_env) ->
        typecheck ~event_env program
        >>= fun _ -> return (program, event_env, expr_env) )
      expecting_error
  ; make_test "6.tdcr"
      (file "exported-events/6.tdcr")
      (fun program (event_env, expr_env) ->
        typecheck ~event_env program
        >>= fun _ -> return (program, event_env, expr_env) )
      expecting_ok ]

(* =============================================================================
   Entry Point
   ============================================================================= *)

let test_suite =
  [ ("simple", simple_set)
  ; ("typechecking", typecheck_set)
  ; ("exported-events", export_events_set) ]

let _ = run "templating" test_suite
