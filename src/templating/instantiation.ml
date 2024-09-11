open Misc.Monads
open Misc.Env
open Syntax

(*
================================================================
  Auxiliary functions
================================================================
*)

(* the alpha-renaming function *)
let rec _counter = ref 0

and _fresh name =
  let res = name ^ "_" ^ (string_of_int !_counter) in
  _counter := !_counter + 1;
  res

(*
================================================================
 Types
================================================================
*)

(*
================================================================
 Error messages
================================================================
*)

and tmpl_not_found id = Error Printf.(sprintf "Template %s not found" id)

(*
================================================================
  Binding templates
================================================================
*)

and bind_tmpls tmpls env = 
  fold_left_result 
    (fun env tmpl -> bind_tmpl tmpl env) 
    env tmpls

and bind_tmpl tmpl env = 
  let id = tmpl.id in
  let expr_env = empty_env in
  (* fold_left_result 
    (fun env (name, _ty) -> 
      Ok (bind name (ref Unit) env)) 
    expr_env tmpl.params
  >>= fun _expr_env -> *)
  Ok (bind id (tmpl, expr_env) env)

(*
================================================================
  Instantiation
================================================================
*)

and instantiate_tmpls tmpl_insts env = 
  fold_left_result 
    (fun inst_program inst -> instantiate_tmpl inst_program inst env)
    empty_subprogram tmpl_insts

and instantiate_tmpl result_program inst env  = 
  let id = inst.tmpl_id in
  match find_flat id env with
  | None -> tmpl_not_found id
  | Some (tmpl, expr_env) ->
    (* TODO: Verify if the length of the args are the same as the params *)
    let (e_ti, q_ti, r_ti) = tmpl.graph in
    (* Events *)
    fold_left_result 
      (instantiate_event inst.args expr_env)
      [] e_ti
    >>= fun events ->

    (* Instantations inside of the Template *)
    instantiate_tmpls q_ti env 
    >>= fun _other_tmpled_programs ->

    (* Relations *)
    fold_left_result 
      (instantiate_relation inst.args)
      [] r_ti
    >>| fun relations ->

    let (result_events, _, result_relations) = result_program in (* Instantiations should be empty! *)
    ( List.flatten [result_events; events], [], List.flatten [result_relations; relations] ) 
    (* FIXME: Maybe this approach could generate many events then necessary *)

and instantiate_event args expr_env tmpl_events target_event =
  (* Bind all arguments to its identifier *)
  fold_left_result
    (fun env (prop, expr) -> Ok (bind prop expr env))
    expr_env args
  >>= fun expr_env -> 

  fold_left_result 
    (replace_event expr_env)
    target_event args
  >>| fun target_event ->
  target_event::tmpl_events

and instantiate_relation _args _tmpl_relations _target_relation =
  Ok (_target_relation::_tmpl_relations)

and replace_event _expr_env event _args  = 
  (* let (id, label) = event.info in
  let event = { event with info = (fresh id, label) } in *)
  Ok event

(*
================================================================
 Entry point
================================================================
*)

and instantiate program = 
  let env = empty_env in
  bind_tmpls program.template_decls env 
  >>= fun env ->

  instantiate_tmpls program.template_insts env
  >>= fun tmpled_program ->

  let (events, _, relations) = tmpled_program in
  let program = { program with
    events = program.events @ events;
    template_insts = [];
    relations = program.relations @ relations;
  } in (* \vec{Q} -> \emptyset *)
  Ok program