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
 Types & Modules
================================================================
*)

module MakeAnnotationEvaluator (T: sig type t end) = struct
  type t = T.t
  
  let when_annotation ~(body: t) ~(none: t) expr expr_env = 
    eval_expr expr expr_env
    >>= fun value -> 
    match value with
    | True -> Ok body
    | False -> Ok none
    | _ -> Error "Invalid annotation value"

  let foreach_annotation ~(body: t) eval_body x expr expr_env =
    print_endline "Foreach annotation";
    print_endline "Expr env:";
    print_endline (string_of_env string_of_expr expr_env);

    eval_expr expr expr_env
    >>= fun value -> 
    match value with
    | List values ->
      fold_left_result
        (fun results value -> 
          eval_expr value expr_env
          >>= fun value ->
          Ok (begin_scope expr_env)
          >>= fun expr_env ->
          Ok (bind x value expr_env)
          >>= fun expr_env ->
          (* Evaluate the body with  *)
          eval_body body expr_env
          >>= fun body ->

            print_endline "Foreach annotation Expr env: ";
            print_endline (string_of_env string_of_expr expr_env);

          Ok (body :: results) )
        [] values
      >>| List.flatten
    | _ -> Error "Invalid foreach expression"
end

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
  print_endline @@ Printf.sprintf "Binding %s: %s" name (string_of_expr expr);
  print_endline @@ Printf.sprintf " in %s\n" (string_of_env string_of_expr env);
  eval_expr expr env
  (* >>= fun value ->
  Ok (bind name value env) *)
  |> function
  | Ok value -> 
    (* print_endline @@ Printf.sprintf " -> %s\n" (string_of_expr value); *)
    Ok (bind name value env)
  | Error _e as err -> 
    print_endline @@ Printf.sprintf "Error: %s" _e;
    err

(*
================================================================
  Instantiation
================================================================
*)

and instantiate_tmpls tmpl_insts tmpl_env expr_env = 
  fold_left_result 
    (fun inst_program inst -> 
      instantiate_tmpl inst_program inst tmpl_env expr_env)
    empty_subprogram tmpl_insts

and instantiate_tmpl result_program inst tmpl_env expr_env  = 
  let id = inst.tmpl_id in
  match find_flat id tmpl_env with
  | None -> tmpl_not_found id
  | Some tmpl ->
    print_endline @@ Printf.sprintf "Instantiating %s --------\n" (string_of_template_inst inst);
    (* TODO: Verify if the length of the args are the same as the params *)
    let (e_ti, q_ti, r_ti) = tmpl.graph in
    let exports_mapping = List.combine tmpl.export inst.x in
    let (result_events, _, result_relations) = result_program in 
    (* Instantiations should be empty! *)

    (* DEBUG: Expr_env*)
    (* print_endline "Expr env:";
    print_endline (string_of_env string_of_expr expr_env); *)

    (* Evaluate the annotations *)
    let program = {
      template_decls = [tmpl];
      events = e_ti;
      template_insts = q_ti;
      relations = r_ti;
    } in
    evaluate_annotations program ~expr_env
    >>= fun { events = e_ti; template_insts = q_ti; relations = r_ti; _ } ->

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

    print_endline "After binding, Expr env:";
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

    (* Fresh ids for the events *)
    fresh_event_ids events relations exports_mapping
    >>| fun (events, relations) ->

    (* Unbind the declared params from the template instance *)
    (* Ok (end_scope expr_env)
    >>| fun _ -> *)

    ( List.flatten [result_events; events; _other_tmpled_events], [], List.flatten [result_relations; relations; _other_tmpled_relations] ) 
    (* |> fun result -> 
    print_endline "-----------------";
    print_endline @@ Printf.sprintf "Result: %s" (string_of_subprogram result);
    result *)
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

(* and replace_event event expr_env   = 
  let { marking; io; _ } = event in
  eval_expr marking.value expr_env
  >>= fun value ->
  print_endline @@ Printf.sprintf "Expr env %s" (string_of_env string_of_expr expr_env);
  print_endline @@ Printf.sprintf "Replacing %s with %s" (string_of_event event) (string_of_expr value);
  begin match io with 
  | Input _ as input -> input
  | Output _ -> Output value
  end |> Result.ok
  >>| fun io ->
    print_endline @@ Printf.sprintf "Replacing %s with %s" (string_of_event_io io) (string_of_expr value);
  { event with marking = { marking with value }; io } *)

and replace_event event expr_env = 
  let{ marking; io; _ } = event in
  let value = match io with
  | Input _ -> marking.value
  | Output expr -> expr in
  eval_expr value expr_env
  >>| fun value -> 
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

and replace_template_inst inst expr_env = 
  let { tmpl_id; args; tmpl_annotations; x } = inst in
  fold_left_result
    (fun result (prop , expr) -> 
      eval_expr expr expr_env
      >>= fun value -> Ok ((prop, value)::result))
    [] args
  >>= fun args ->
  Ok { tmpl_id; args; tmpl_annotations; x }

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
 Annotation Evaluation
================================================================
*)

and analize_annotations_of_event event ~none annotations expr_env = 
  print_endline "Analizing annotations of events";
  fold_left_result
    (fun result annotation -> 
      analize_annotation_event event ~none annotation expr_env
      >>= fun event -> Ok (event::result))
    [] annotations

and analize_annotation_event event ~none annotation expr_env =
  let module AnnotationEvaluator = MakeAnnotationEvaluator(struct type t = event list end) in
  match annotation with 
  | When expr ->
    AnnotationEvaluator.when_annotation ~body:event ~none expr expr_env 
  | Foreach (x, expr) -> 
    AnnotationEvaluator.foreach_annotation ~body:event 
    (fun body expr_env -> 
      fold_left_result
        (fun result event -> 
          replace_event event expr_env
          >>| fun event -> 
            print_endline @@ Printf.sprintf "Event: %s" (string_of_event event);
            event::result)
      [] body)
     x expr expr_env
    >>= fun events -> 
      print_endline "Foreach annotation Event Result: ";
      print_endline @@ String.concat "\n" (List.map string_of_event events);
      Ok events

and analize_annotations_of_relation relation ~none annotations expr_env = 
  print_endline "Analizing annotations of relations";
  fold_left_result
    (fun result annotation -> 
      analize_annotation_relation relation ~none annotation expr_env
      >>= fun relation -> Ok (relation::result))
    [] annotations

and analize_annotation_relation relation ~none annotation expr_env =
  let module AnnotationEvaluator = MakeAnnotationEvaluator(struct type t = relation list end) in
  match annotation with 
  | When expr -> 
    AnnotationEvaluator.when_annotation ~body:relation ~none expr expr_env
  | Foreach (x, expr) -> 
    AnnotationEvaluator.foreach_annotation ~body:relation 
    (fun body expr_env -> 
      fold_left_result
        (fun result relation -> 
          replace_relation relation expr_env
          >>| fun relation -> relation::result)
      [] body)
     x expr expr_env

and analize_annotations_of_inst instance ~none annotations expr_env = 
  print_endline "Analizing annotations of insts";
  fold_left_result
    (fun result annotation -> 
      analize_annotation_inst instance ~none annotation expr_env
      >>= fun instance -> Ok (instance::result))
    [] annotations

and analize_annotation_inst instance ~none annotation expr_env =
  let module AnnotationEvaluator = MakeAnnotationEvaluator(struct type t = template_instance list end) in
  match annotation with 
  | When expr -> 
    AnnotationEvaluator.when_annotation ~body:instance ~none expr expr_env
  | Foreach (x, expr) -> 
    AnnotationEvaluator.foreach_annotation ~body:instance 
    (fun instances _expr_env -> 
      fold_left_result
        (fun result isnt ->
          replace_template_inst isnt expr_env
          >>| fun inst -> inst::result)
        [] instances)
        x expr expr_env

and deannotate_events events = 
  fold_left_result
    (fun result event -> 
      deannotate_event event
      >>= fun event -> Ok (event::result))
    [] events

and deannotate_event event =
  Ok { event with annotations = [] }

and deannotate_template_insts insts = 
  fold_left_result
    (fun result inst -> 
      deannotate_template_inst inst
      >>= fun inst -> Ok (inst::result))
    [] insts

and deannotate_template_inst inst =
  Ok { inst with tmpl_annotations = [] }

and deannotate_relations relations = 
  fold_left_result
    (fun result relation -> 
      deannotate_relation relation
      >>= fun relation -> Ok (relation::result))
    [] relations

and deannotate_relation relation =
  match relation with
  | SpawnRelation (from, guard, subprogram, _) -> Ok (SpawnRelation (from, guard, subprogram, []))
  | ControlRelation (from, guard, dest, t, _) -> Ok (ControlRelation (from, guard, dest, t, []))

and evaluate_annotations ?(expr_env = empty_env) program  =
  (* Evaluate events *)
  let events = program.events in
  let annotation_of_event event = event.annotations in
  fold_left_result
    (fun result event -> 
      analize_annotations_of_event [event] ~none:[] (annotation_of_event event) expr_env
      >>= function
      | [events] -> Ok (List.flatten [result; events])
      | [] -> Ok (event::result)
      (* Grouping annotations is not available rn, didn't have the time to implement, don't blame me... :) *)
      | _ -> failwith "Unsuported annotation")
    [] events
  >>= deannotate_events
  >>= fun events ->
      print_endline @@ Printf.sprintf "Events: %s" (String.concat "\n" (List.map string_of_event events));


  (* Evaluate instantiations *)
  let insts = program.template_insts in
  let annotation_of_inst inst = inst.tmpl_annotations in
  fold_left_result
    (fun result inst -> 
      analize_annotations_of_inst [inst] ~none:[] (annotation_of_inst inst) expr_env
      >>= function
      | [inst] -> Ok (List.flatten [result; inst])
      | [] -> Ok (inst::result)
      (* In case of grouping annotations is not available rn, didn't have the time to implement, don't blame me... :) *)
      | _ -> failwith "Unsuported annotation")
    [] insts
  >>= deannotate_template_insts
  >>= fun template_insts ->
  (* print_endline @@ Printf.sprintf "Template insts: %s" (String.concat "\n" (List.map string_of_template_inst template_insts)); *)

  (* Evaluate relations *)
  let relations = program.relations in
  let annotation_of_relation relation = 
    match relation with 
    | SpawnRelation (_, _, _, annot) -> annot
    | ControlRelation (_, _, _, _, annot) -> annot in
  fold_left_result
    (fun result relation -> 
      analize_annotations_of_relation [relation] ~none:[] (annotation_of_relation relation) expr_env
      >>= function
      | [relation] -> Ok (List.flatten [result; relation])
      | [] -> Ok (relation::result)
      (* In case of grouping annotations is not available rn, didn't have the time to implement, don't blame me... :) *)
      | _ -> failwith "Unsuported annotation")
    [] relations
  >>= deannotate_relations
  >>| fun relations ->
  (* Put it together *)
  { program with events ; template_insts; relations }



(* module AnnotationEvaluator = sig
  type t
  val when_annotation: t list -> expr -> expr env -> t list 
  val foreach_annotation: t list -> string -> expr -> expr env -> t list
end *)


(*
================================================================
 Entry point
================================================================
*)

let instantiate ?(expr_env = empty_env) program  = 
  (* Bind all the available templates of the program *)
  bind_tmpls program.template_decls empty_env 
  >>= fun tmpl_env ->

  (* Evaluate the annotations *)
  evaluate_annotations program ~expr_env
  >>= fun program -> 

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
