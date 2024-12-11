open Alcotest
open Misc
open Monads.ResultMonad

(* open Env *)
open Templating
open Syntax
open Api
open Program_helper
open Instantiation
open Runtime
open Common

let tests_folder = "test/files/simple"

let file name = Printf.sprintf "%s/%s" tests_folder name

let test_suite =
  (* Note: The following tests assume that the programs are typed correctly. *)
  [ ( "basic"
    , [ make_test "0.tdcr" (file "0.tdcr")
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
            check_int "Expected 0 template instantiation" 0
              (List.length g_insts) ;
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
      ; make_test "1.tdcr" (file "1.tdcr")
          (fun program (event_env, expr_env) ->
            instantiate ~expr_env ~event_env program
            >>= fun (program, event_env, expr_env) ->
            check_true "Event a not found"
              (has_event ~filter:(same_id "a") program) ;
            check_true "Event a doesn't have any relations"
              (has_relation "a" program) ;
            execute ~event_id:"a" ~expr:(IntLit 1) ~expr_env ~event_env program
            >>= fun (program, event_env, expr_env) ->
            check_true "Expected 3 events" (List.length program.events = 3) ;
            check_true "Expected 2 relations" (List.length program.relations = 2) ;
            check_true "Not expecting instantiations to be done."
              (List.length program.template_insts = 0) ;
            check_true "Event a not found"
              (has_event ~filter:(same_id "a") program) ;
            check_true "Event b not found"
              (has_event ~filter:(same_id "b") program) ;
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
      ; make_test "2.tdcr" (file "2.tdcr")
          (fun program (event_env, expr_env) ->
            check_int "Expected 1 event" 1 (List.length program.events) ;
            check_int "Expected 0 template instantiation" 0
              (List.length program.template_insts) ;
            check_int "Expected 2 relations" 2 (List.length program.relations) ;
            let spawn_relations =
              find_all_relations ~filter:is_spawn "a" program
            in
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
          expecting_ok ] ) ]

let _ = run ~verbose:false "templating" test_suite
