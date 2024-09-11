open Misc.Monads
open Misc.Env
open Syntax

(*
================================================================
  Auxiliary functions
================================================================
*)

(* the alpha-renaming function *)
let rec counter = ref 0

and fresh name =
  let res = name ^ "_" ^ (string_of_int !counter) in
  counter := !counter + 1;
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
  fold_left_result 
    (fun env (name, _ty) -> 
      Ok (bind name (ref Unit) env)) 
    expr_env tmpl.params
  >>= fun _expr_env -> (* NOT USED *)
  Ok (bind id tmpl env)

and bind_param (name, _ty) env = 
  bind (fresh name) (ref Unit) env

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
  let (result_events, _, result_relations) = result_program in (* Instantiations should be empty! *)
  let id = inst.tmpl_id in
  match find_flat id env with
  | None -> tmpl_not_found id
  | Some tmpl ->
    (* TODO: Verify if the length of the args are the same as the params *)
    let (e_ti, q_ti, r_ti) = tmpl.graph in
    (* Events *)
    fold_left_result 
      (instantiate_event inst.args)
      [] e_ti
    >>= fun events ->

    (* Instantations inside of the Template *)
    instantiate_tmpls q_ti env 
    >>= fun _other_tmpled_programs ->

    (* Relations *)
    fold_left_result 
      (instantiate_relation inst.args)
      [] r_ti
    >>= fun relations ->
  Ok ( List.flatten [result_events; events], [], List.flatten [result_relations; relations] ) 
  (* FIXME: Maybe this approach could generate many events then necessary *)

and instantiate_event _args _tmpl_events _target_event =
  fold_left_result 
    replace_all_event
    _target_event _args
  >>= fun _target_event ->
  let (id, label) = _target_event.info in
  let _target_event = { _target_event with info = (fresh id, label) } in
  Ok (_target_event::_tmpl_events)

and instantiate_relation _args _tmpl_relations _target_relation =
  Ok (_target_relation::_tmpl_relations)




and replace_all_event target_event (prop, expr) =
  replace_id target_event prop expr
  >>= fun target_event ->
  replace_expr target_event prop expr
  >>= fun target_event ->
  Ok target_event

(* FIXME: This is correct??? *)
and replace_id target_event prop expr = 
  let (id, label) = target_event.info in
  if id = prop then 
    (* FIXME: Evaluation method! *)
    match expr with 
    | Identifier id -> Ok { target_event with info = (id, label) }
    (* ... *)
    | _ -> Ok target_event
    (* --- *)
  else 
    Ok target_event

and replace_expr target_event prop expr = 
  match target_event.io with
  | Output e -> 
    (* FIXME: Evaluation method! *)
    begin match e with
      | Identifier id -> 
        if id = prop then 
          let value = target_event.marking.value in
          value := expr;
          Ok target_event
        else Ok target_event
      (* ... *)
      | _ -> Ok target_event
    end
    (* --- *)
  | _ -> Ok target_event

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

  print_endline "= Instantiation done =";
  print_endline @@ string_of_subprogram tmpled_program;
  print_endline "======================";

  let (events, _, relations) = tmpled_program in
  let program = { program with
    events = program.events @ events;
    template_insts = [];
    relations = program.relations @ relations;
  } in (* \vec{Q} -> \emptyset *)
  Ok program