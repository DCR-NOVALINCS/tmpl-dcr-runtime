open Misc.Env
open Misc.Monads
open Syntax
(* open Templating.Errors *)

(*
=============================================================================
  Aux functions & types
=============================================================================
*)  


(*
=============================================================================
  Main Typechecking functions
=============================================================================
*)  

let rec typecheck program = 
  let env = empty_env in
  let rec typecheck_aux remaining_symbols program env = 
    typecheck_template_defs program.template_decls env
    >>= fun _ -> 
    typecheck_events program.events env 
    >>= fun _ ->
    typecheck_template_insts program.template_insts env
    >>= fun _ ->
    typecheck_relations program.relations env
    >>= fun _ -> 
    match remaining_symbols with
    | [] -> Ok ()
    | _ -> typecheck_aux remaining_symbols program env 
  in typecheck_aux [] program env

and typecheck_template_defs _template_defs _env = Ok ()

and typecheck_events _events _env = Ok ()

and typecheck_template_insts _template_insts _env = Ok ()

and typecheck_relations _relations _env = Ok ()

and typecheck_expr _expr _env = Ok ()
