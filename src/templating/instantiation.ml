open Misc.Monads
open Misc.Env
open Syntax

(*
================================================================
 Error messages
================================================================
*)

let rec tmpl_not_found id = Error Printf.(sprintf "Template %s not found" id)

(*
================================================================
  Binding templates
================================================================
*)

and bind_tmpls tmpls env = 
  fold_left_result (fun env tmpl -> bind_tmpl tmpl env) env tmpls

and bind_tmpl tmpl env = 
  let id = tmpl.id in
  Ok (bind id tmpl env)

(*
================================================================
  Instantiation
================================================================
*)

and instantiate_tmpls program _ = 
  let tmpl_insts = program.template_insts in
  fold_left_result instantiate_tmpl program tmpl_insts
  >>= fun program ->
  Ok program

and instantiate_tmpl program _ = 
  Ok program

(*
================================================================
 Entry point
================================================================
*)

and instantiate program = 
  let env = empty_env in
  bind_tmpls program.template_decls env 
  >>= fun env ->
  instantiate_tmpls program env