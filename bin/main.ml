open Templating.Syntax
open Templating.Runtime
open Templating.Battery_tests
open Templating.Instantiation
open Misc.Monads
(* open Misc.Env *)

(*
=============================================================================
  Main Section
=============================================================================
*)  

let _ = 
  let target = _test6 in
  ( 
    Ok target
    >>= fun program ->
    preprocess_program program
    >>= fun (event_env, expr_env) ->
    instantiate ~expr_env program
    >>= fun (program, expr_env) ->
    execute ~event_env ~expr_env ~event_id:"a" program
    >>= fun program -> 
    view ~event_env ~expr_env ~should_print_relations:true program 
  )
  |> function
  | Error _e -> 
    print_endline _e;
    print_endline "-----------------\n";
    print_endline @@ string_of_program target
  | _ -> ()

