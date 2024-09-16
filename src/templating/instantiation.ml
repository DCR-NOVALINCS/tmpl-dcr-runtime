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

and invalid_annotation_value value ty = Error Printf.(sprintf "Invalid annotation value %s for type %s" (string_of_expr value) (string_of_type_expr ty))

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
  Ok (bind id tmpl env)

and bind_arg (name, expr) env = 
  (* print_string @@ Printf.sprintf "Binding %s: %s" name (string_of_expr expr); *)
  eval_expr expr env
  |> function
  | Ok value -> 
    (* print_endline @@ Printf.sprintf " -> %s\n" (string_of_expr value); *)
    Ok (bind name value env)
  | Error _e as err -> 
    err

(*
================================================================
  Instantiation
================================================================
*)

and instantiate_tmpls tmpl_insts tmpl_env expr_env = 
  (* FIXME: Move this inside of the [instantiate_tmpl] function *)
  fold_left_result
    (fun final_insts inst -> 
      analize_annotations [inst] inst.tmpl_annotations expr_env
      >>| List.append final_insts)
    [] tmpl_insts 
  >>= fun tmpl_insts ->
  fold_left_result 
    (fun inst_program inst -> 
      instantiate_tmpl inst_program inst tmpl_env expr_env)
    empty_subprogram tmpl_insts

and instantiate_tmpl result_program inst tmpl_env expr_env  = 
  let id = inst.tmpl_id in
  match find_flat id tmpl_env with
  | None -> tmpl_not_found id
  | Some tmpl ->
    print_endline @@ Printf.sprintf "Instantiating %s(%s) => %s--------\n" 
    id 
    (String.concat ", " (List.map (fun (name, expr) -> name ^ " = " ^ (string_of_expr expr)) inst.args))
    (String.concat ", " inst.x)
    ;
    (* TODO: Verify if the length of the args are the same as the params *)
    let (e_ti, q_ti, r_ti) = tmpl.graph in
    let exports_mapping = List.combine tmpl.export inst.x in
    let (result_events, _, result_relations) = result_program in 
    (* Instantiations should be empty! *)

    (* DEBUG: Expr_env*)
    print_endline "Expr env:";
    print_endline (string_of_env string_of_expr expr_env);

    (* Bind all arguments to its identifier *)
    Ok (begin_scope expr_env)
    >>= fun expr_env ->
    fold_left_result
      (fun env event -> 
        let (id, _) = event.info in
        bind_arg (id, record_event event) env)
      expr_env e_ti
    >>= fun expr_env -> 
    fold_left_result
      (fun env (prop, expr) -> bind_arg (prop, expr) env)
      expr_env inst.args
    >>= fun expr_env -> 
    
    



    (* DEBUG: Expr_env*)
    print_endline "After binding:";
    print_endline (string_of_env string_of_expr expr_env);

    
    (* Instantiate events *)
    fold_left_result 
      (instantiate_event expr_env)
      [] e_ti
    >>= fun events ->

    (* Maps the exported events *)
    (* TODO: This should also affect the events from the args! *)
    export_map_events events exports_mapping
    >>= fun events ->
    
    (* Instantations inside of the Template *)
    instantiate_tmpls q_ti tmpl_env expr_env
    >>= fun (_other_tmpled_events, _, _other_tmpled_relations) ->

    (* Instantiate relations *)
    fold_left_result 
      (instantiate_relation expr_env)
      [] r_ti
    >>= fun relations ->

    (* Filter/Iterate the relations based on its annotations  *)
    (* eval_relation_template_annotations relations expr_env
    >>= fun relations ->  *)

    (* Fresh ids for the events *)
    fresh_event_ids events relations exports_mapping
    >>= fun (events, relations) ->

    (* Unbind the declared params from the template instance *)
    Ok (end_scope expr_env)
    >>| fun _ ->

    ( List.flatten [result_events; events; _other_tmpled_events], [], List.flatten [result_relations; relations; _other_tmpled_relations] ) 
    (* FIXME: Maybe this approach could generate many events then necessary *)

and instantiate_event _expr_env tmpl_events target_event  =
(* Filter/Iterate the events based on its annotations  *)
  replace_event target_event _expr_env
  >>| fun target_event ->
  target_event :: tmpl_events

and instantiate_relation _expr_env _tmpl_relations _target_relation =
  replace_relation _target_relation _expr_env
  >>| fun _target_relation ->
  _target_relation::_tmpl_relations

and replace_event event _expr_env   = 
  let { marking; io; _ } = event in
  eval_expr marking.value _expr_env
  >>= fun value ->
  begin match io with 
  | Input _ as input -> input
  | Output _ -> Output value
  end |> Result.ok
  >>| fun io ->
  { event with marking = { marking with value }; io }

and replace_relation relation _expr_env = 
  match relation with
  | SpawnRelation (from, guard, subprogram, _annot) -> 
    eval_expr guard _expr_env
    >>| fun guard ->
    begin match find_flat from _expr_env with
    | Some (Identifier id) -> id
    | _ -> from end
    |> fun from ->
    SpawnRelation (from, guard, subprogram, _annot)
  | ControlRelation (from, guard, dest, t, _annot) -> 
    eval_expr guard _expr_env
    >>| fun guard ->
    begin match find_flat from _expr_env with
    | Some (Identifier id) -> id
    | _ -> from end
    |> fun from ->
    begin match find_flat dest _expr_env with
    | Some (Identifier id) -> id
    | _ -> dest end
    |> fun dest ->
    ControlRelation (from, guard, dest, t, _annot)

and when_annotation body expr expr_env = 
  (* Evaluate [expr] *)
  (eval_expr expr expr_env
  >>= fun value -> 

  print_endline @@ Printf.sprintf "When annotation: %s" (string_of_expr value);
  (* Check if the [expr] evaluates to True, if so return [body], return empty otherwise *)
  begin match value with
  | True -> Ok body
  | _ -> Ok []
  end) |> Result.to_option  
and foreach_annotation body eval_body x expr expr_env =
  (* Evaluate [expr] *)
  eval_expr expr expr_env
  >>= fun value -> 

  (* Check if [expr] is a list *)
  match value with
  | List values ->
    fold_left_result
      (fun results value -> 
        (* Evaluate [value] *)
        eval_expr value expr_env
        >>= fun value ->
        
        (* Bind [x] with the expr of [value] *)
        Ok (bind x value expr_env)
        >>= fun expr_env ->
        
        (* Evaluate [body] *)
        eval_body body expr_env
        >>= fun body ->
        
        (* Append the result in the acc [results] *)
        Ok (body::results) )
      [] values
    >>| List.flatten
  | _ -> invalid_annotation_value value (ListTy (UnitTy))
 
and analize_annotations body annotations expr_env = 
  print_endline "Analizing annotations";
  fold_left_result
    (fun result annotation -> 
      analize_annotation body annotation expr_env
      >>= fun body -> Ok (body::result))
    [] annotations
  >>| List.flatten

and analize_annotation body annotation expr_env =
  match annotation with 
  | When expr -> 
    begin match when_annotation body expr expr_env with
    | Some body -> Ok body
    | None -> Ok []
    end
  | Foreach (x, expr) ->
    (* TODO: Function to evaluate and change [body] *)
    foreach_annotation body (fun body _expr_env -> Ok body) x expr expr_env

and export_map_events events export_mapping =
  fold_left_result
    (map_event_id export_mapping)
    [] events

and map_event_id exports_mapping events event  = 
  let (id, label) = event.info in
  begin match List.assoc_opt id exports_mapping with
  | None -> event
  | Some new_id -> { event with info = (new_id, label) }
  end |> Result.ok
  >>| fun event -> event::events

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
  Ok ({ program with
    events = List.append program.events events;
    template_insts = [];
    relations = List.append program.relations relations;
  }, expr_env)
