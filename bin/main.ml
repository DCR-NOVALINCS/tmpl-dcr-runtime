open Templating.Syntax
open Templating.Runtime
open Templating.Battery_tests
open Templating.Instantiation
open Misc.Monads
(* open Misc.Env *)


(*
=============================================================================
  Aux functions
=============================================================================
*)

let _add x y = BinaryOp (IntLit x, IntLit y, Add)

(*
=============================================================================
  Main Section
=============================================================================
*)  

let _ = 
  let target = _test4 in
  ( 
    Ok target
    >>= fun program ->
    preprocess_program program
    >>= fun (event_env, expr_env) ->
    instantiate ~expr_env program
    >>= fun (program, expr_env) ->
    (* execute ~event_env ~expr_env ~event_id:"a'" program
    >>= fun program ->  *)
    view ~event_env ~expr_env ~should_print_relations:true program 
  )
  |> function
  | Error e -> 
    print_endline e;
    print_endline "-----------------\n";
    print_endline @@ string_of_program target
  | _ -> ()

