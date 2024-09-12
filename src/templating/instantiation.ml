open Misc.Monads
open Misc.Env
open Syntax
open Evaluation

(*
================================================================
  Auxiliary functions
================================================================
*)

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

let rec tmpl_not_found id = Error Printf.(sprintf "Template %s not found" id)

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
  (* let expr_env = empty_env in
  fold_left_result 
    (fun env (name, _ty) -> 
      Ok (bind name Unit env)) 
    expr_env tmpl.params
  >>= fun expr_env -> *)
  Ok (bind id tmpl env)

and _bind_arg (name, expr) env = 
  eval_expr expr env
  |> function
  | Ok value -> Ok (bind name value env)
  | Error e -> 
    print_endline e;
    Error e

(*
================================================================
  Instantiation
================================================================
*)

and instantiate_tmpls tmpl_insts tmpl_env expr_env = 
  fold_left_result 
    (fun inst_program inst -> instantiate_tmpl inst_program inst tmpl_env expr_env)
    empty_subprogram tmpl_insts

and instantiate_tmpl result_program inst tmpl_env expr_env  = 
  let id = inst.tmpl_id in
  print_endline @@ Printf.sprintf "Instantiating %s(%s)" id (String.concat ", " (List.map (fun (name, expr) -> name ^ " = " ^ (string_of_expr expr)) inst.args));
  match find_flat id tmpl_env with
  | None -> tmpl_not_found id
  | Some tmpl ->
    (* TODO: Verify if the length of the args are the same as the params *)
    let (e_ti, q_ti, r_ti) = tmpl.graph in
    (* Events *)
    fold_left_result 
      (instantiate_event inst.args expr_env)
      [] e_ti
    >>= fun events ->

    (* Instantations inside of the Template *)
    instantiate_tmpls q_ti tmpl_env expr_env
    >>= fun _other_tmpled_programs ->

    (* Relations *)
    fold_left_result 
      (instantiate_relation inst.args)
      [] r_ti
    >>| fun relations ->

    let (result_events, _, result_relations) = result_program in (* Instantiations should be empty! *)
    ( List.flatten [result_events; events], [], List.flatten [result_relations; relations] ) 
    (* FIXME: Maybe this approach could generate many events then necessary *)

and instantiate_event _args _expr_env tmpl_events target_event =
  print_endline @@ string_of_env string_of_expr _expr_env;
  (* Bind all arguments to its identifier *)
  fold_left_result
    (fun env (prop, expr) -> _bind_arg (prop, expr) env)
    _expr_env _args
  >>= fun _expr_env -> 
    print_endline "Arguments evaluated!";
    print_endline @@ string_of_env string_of_expr _expr_env;
    print_endline @@ Printf.sprintf "Event Result %s" (string_of_event target_event);
  (* fold_left_result 
    (replace_event expr_env)
    target_event args
  >>| fun target_event -> *)
  Ok (target_event :: tmpl_events)


and instantiate_relation _args _tmpl_relations _target_relation =
  Ok (_target_relation::_tmpl_relations)

(* and replace_event _expr_env event _args  = 
  (* let (id, label) = event.info in
  let event = { event with info = (fresh id, label) } in *)
  Ok event *)

(*
================================================================
 Entry point
================================================================
*)

and instantiate ?(expr_env = empty_env) program  = 
  (* Bind all the available templates of the program *)
  bind_tmpls program.template_decls empty_env 
  >>= fun tmpl_env ->

  (* Instantiate all the instantiations of the program *)
  instantiate_tmpls program.template_insts tmpl_env expr_env
  >>= fun tmpled_program ->

  (* Append the result in the program *)
  let (events, _, relations) = tmpled_program in
  Ok { program with
    events = List.append program.events events;
    template_insts = [];
    relations = List.append program.relations relations;
  } (* \vec{Q} -> \emptyset *)
