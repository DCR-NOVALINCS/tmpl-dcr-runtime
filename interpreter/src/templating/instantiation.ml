open Misc.Monads
open Misc.Env
open Misc.Printing
open Syntax
open Evaluation
open Errors

(*
================================================================
  Auxiliary functions
================================================================
*)

let rec remove_tail list =
  match list with
  | [] -> []
  | [_] -> []
  | x::xs -> x::(remove_tail xs) 

(*
================================================================
 Types & Modules
================================================================
*)

module MakeAnnotationEvaluator (T: sig type t end) = struct
  type body = T.t
  
  let when_annotation ~(body: body) ~(none: body) expr expr_env = 
    eval_expr expr expr_env
    >>= fun value -> 
    match value.data with
    | True -> Ok body
    | False -> Ok none
    | _ -> invalid_annotation_value value BoolTy

  let foreach_annotation ~(body: body) eval_body x expr expr_env =
    eval_expr expr expr_env
    >>= fun value -> 
    match value.data with
    | List values ->
      map_result
        (fun value -> eval_body expr_env x value body )
        values
      >>| List.flatten
    | _ -> 
      let ty = match !(expr.ty) with
      | None -> UnitTy
      | Some ty -> ty in
      invalid_annotation_value value (ListTy (annotate ty))
end

(*
================================================================
  Binding templates
================================================================
*)

let rec bind_tmpls tmpls env = 
  fold_left_result 
    (fun env tmpl -> bind_tmpl tmpl env) 
    env tmpls

and bind_tmpl tmpl env = 
  let id = tmpl.id in
  Logger.info @@ Printf.sprintf "Binding template %s" (CString.colorize ~color:Yellow id.data);
  Ok (bind id.data tmpl env)

and bind_arg (name, expr) env = 
  Logger.debug @@ Printf.sprintf "Binding argument %s" (CString.colorize ~color:Yellow name);
  eval_expr expr env
  >>| fun value -> bind name value env

(*
================================================================
  Instantiation
================================================================
*)

and instantiate_tmpls tmpl_insts tmpl_env expr_env = 
  (* Logger.info "Instantiating templates"; *)
  fold_left_result 
    (fun inst_program inst -> 
      instantiate_tmpl inst_program inst tmpl_env expr_env)
    empty_subprogram tmpl_insts

and instantiate_tmpl result_program inst tmpl_env expr_env  = 
  let id = inst.tmpl_id in
  match find_flat id.data tmpl_env with
  | None -> tmpl_not_found id
  | Some tmpl ->
    Logger.info @@ Printf.sprintf "Instantiating %s" (CString.colorize ~color:Yellow @@ string_of_template_inst inst);
    let (e_ti, q_ti, r_ti) = tmpl.graph in
    let (result_events, _, result_relations) = result_program in 
    (* Instantiations should be empty! *)

    (* Bind all arguments to its identifier *)
    Ok (begin_scope expr_env)
    >>= fun expr_env ->
    fold_left_result
      (fun env event -> 
        let (id, _) = event.data.info in
        bind_arg (id.data, record_event event) env)
      expr_env e_ti
    >>= fun expr_env -> 
      Logger.debug "After binding the events";
      Logger.debug @@ string_of_env string_of_expr expr_env;

    (* Bind all the arguments to its identifier *)
    (* FIXME: Check if the template has default value for each parameter! *)
    fold_left_result
      (fun env (prop, expr) -> bind_arg (prop.data, expr) env)
      expr_env inst.args
    >>= fun expr_env -> 
      Logger.debug "After binding the args";
      Logger.debug @@ string_of_env string_of_expr expr_env;

    (* Evaluate the annotations *)
    let program = {
      template_decls = [tmpl];
      events = e_ti;
      template_insts = q_ti;
      relations = r_ti;
    } in
    evaluate_annotations program ~expr_env
    >>= fun { events = e_ti; template_insts = q_ti; relations = r_ti; _ } ->


    (* Instantiate events *)
    map_result
      (fun e -> instantiate_event expr_env e)
      e_ti
    >>= fun events ->

    (* Maps the exported events *)
    (* TODO: This should also affect the events from the args! *)
    (* export_map_events events exports_mapping
    >>= fun events -> *)
    
    (* Instantations inside of the Template *)
    instantiate_tmpls q_ti tmpl_env expr_env
    >>= fun (other_tmpled_events, _, other_tmpled_relations) ->

    (* Instantiate relations *)
    map_result
      (fun r -> instantiate_relation expr_env r)
      r_ti
    >>= fun relations ->

    (* Fresh ids for the events *)
    begin match List.length inst.x = List.length tmpl.export with
    | true -> Ok ()
    | false -> invalid_number_of_exported_events inst.x tmpl.export
    end
    >>= fun _ ->
    let exports_mapping = List.combine (deannotate_list tmpl.export) inst.x in
    fresh_event_ids events relations exports_mapping
    >>| fun (events, relations) ->

    Logger.debug "After freshing the ids";
    Logger.debug @@ "Relations: " ^ (List.map string_of_relation relations |> String.concat "\n");
    (* Unbind the declared params from the template instance *)
    (* Ok (end_scope expr_env)
    >>| fun _ -> *)

    ( List.flatten [result_events; events; other_tmpled_events], [], List.flatten [result_relations; relations; other_tmpled_relations] ) 

and instantiate_event expr_env target_event  =
(* Filter/Iterate the events based on its annotations  *)
  replace_event target_event expr_env

and instantiate_relation expr_env target_relation =
  replace_relation target_relation expr_env

and replace_event event expr_env = 
  let{ marking; io; _ } = event.data in
  let value = match io.data with
  | Input _ -> marking.data.value
  | Output expr -> expr in
  eval_expr value expr_env
  >>| fun value -> 
  let marking = { marking with data = { marking.data with value } } in
  let io = begin match io.data with
  | Input _ as input -> input
  | Output _ -> Output value end |> annotate ~loc:io.loc ~ty:!(io.ty) in
  { event with data = { event.data with marking; io } }


and replace_relation relation expr_env = 
  (* FIXME: Can't replace the [from] and [dest] ids *)
  match relation.data with
  | SpawnRelation (from, guard, subprogram, annot) -> 
    eval_expr guard expr_env
    >>= fun guard ->
    begin match find_flat from.data expr_env with
    | Some { data = (Identifier id); _ } -> id
    | _ -> from end
    |> fun from ->
    Ok { relation with data = (SpawnRelation (from, guard, subprogram, annot)) }
  | ControlRelation (from, guard, dest, t, annot) -> 
    eval_expr guard expr_env
    >>= fun guard ->
      Logger.debug @@ Printf.sprintf "From: %s" from.data;
      Logger.debug @@ string_of_env string_of_expr expr_env;
    begin match find_flat from.data expr_env with
    | Some { data = (Identifier id); _ } -> id
    | Some { data = (Record [({ data = prop_name; _}, _)]); _} when prop_name = "value" -> 
      (* This case is weird... *)
      Logger.debug "From is a record with the property value";
      from
    | _ ->
      Logger.warn "From not found in the expr_env";
      Logger.warn @@ Printf.sprintf "Found %s" (string_of_expr (find_flat from.data expr_env |> Option.get));
      from end
    |> fun from ->
            Logger.debug @@ from.data;
    begin match find_flat dest.data expr_env with
    | Some { data = (Identifier id); _ } -> id
    | _ -> dest end
    |> fun dest ->
    Logger.debug @@ string_of_relation relation;
    Logger.debug @@ string_of_relation { relation with data = (ControlRelation (from, guard, dest, t, annot)) };
    Ok { relation with data = (ControlRelation (from, guard, dest, t, annot)) }

and replace_template_inst inst expr_env = 
  let { tmpl_id; args; tmpl_annotations; x } = inst in
  map_result
    (fun (prop , expr) -> 
      eval_expr expr expr_env
      >>| fun value -> (prop, value))
    args
  >>= fun args ->
  Ok { tmpl_id; args; tmpl_annotations; x }

and export_map_events events export_mapping =
  List.map (fun event -> map_event_id event export_mapping
  ) events |> Result.ok

and map_event_id event export_mapping = 
  let (id, label) = event.data.info in
  match List.assoc_opt id.data export_mapping with
  | None -> event
  | Some new_id -> { event with data = { event.data with info = (new_id, label) } }

(*
================================================================
 Annotation Evaluation
================================================================
*)

and analize_annotations_of_event event ~none annotations expr_env = 
  (* print_endline "Analizing annotations of events"; *)
  fold_left_result
    (fun result annotation -> 
      analize_annotation_event event ~none annotation expr_env
      >>= fun event -> Ok (event::result))
    [] annotations

and analize_annotation_event event ~none annotation expr_env =
  Logger.info @@ Printf.sprintf "Analizing annotation %s of the event %s" (string_of_template_annotation annotation |> CString.colorize ~color:Yellow) (List.map string_of_event event |> String.concat ", " |> CString.colorize ~color:Yellow);
  let module AnnotationEvaluator = MakeAnnotationEvaluator(struct type t = event list end) in
  match annotation with 
  | When expr ->
    AnnotationEvaluator.when_annotation ~body:event ~none expr expr_env 
  | Foreach (x, expr) -> 
    AnnotationEvaluator.foreach_annotation ~body:event 
    (fun expr_env x value body -> 
      map_result
        (fun event -> 
          Ok (begin_scope expr_env)
          >>= fun expr_env ->
          Ok (bind x.data value expr_env)
          >>= fun expr_env ->
          replace_event event expr_env
          )
      body)
     x expr expr_env

and analize_annotations_of_relation relation ~none annotations expr_env = 
  (* print_endline "Analizing annotations of relations"; *)
  fold_left_result
    (fun result annotation -> 
      analize_annotation_relation relation ~none annotation expr_env
      >>= fun relation -> Ok (relation::result))
    [] annotations

and analize_annotation_relation relation ~none annotation expr_env =
  Logger.info @@ Printf.sprintf "Analizing annotation %s of the relation %s" (string_of_template_annotation annotation |> CString.colorize ~color:Yellow) (List.map string_of_relation relation |> String.concat ", " |> CString.colorize ~color:Yellow);
  let module AnnotationEvaluator = MakeAnnotationEvaluator(struct type t = relation list end) in
  match annotation with 
  | When expr -> 
    AnnotationEvaluator.when_annotation ~body:relation ~none expr expr_env
  | Foreach (x, expr) -> 
    AnnotationEvaluator.foreach_annotation ~body:relation 
    (fun expr_env x value body -> 
      map_result
        (fun relation -> 
          Ok (begin_scope expr_env)
          >>= fun expr_env ->
          Ok (bind x.data value expr_env)
          >>= fun expr_env ->
          replace_relation relation expr_env
          )
      body)
     x expr expr_env

and analize_annotations_of_inst instance ~none annotations expr_env = 
  (* print_endline "Analizing annotations of insts"; *)
  Logger.group "Analizing annotations of insts";
  Logger.debug @@ Printf.sprintf "Expr env: %s" (string_of_env string_of_expr expr_env);
  fold_left_result
    (fun (result, expr_env) annotation -> 
      analize_annotation_inst instance ~none annotation expr_env
      
      (* Deannotate analized annotation *)
      (* >>= fun result_instance ->   
      map_result
        (fun inst -> 
          Logger.debug @@ Printf.sprintf "Deannotating %s" (string_of_template_inst inst);
          Ok { inst with tmpl_annotations = List.tl inst.tmpl_annotations } )
        instance *)
      >>= fun instance ->
        Logger.debug @@ Printf.sprintf "Deannotated %s" (List.map string_of_template_inst instance |> String.concat "\n");
      Ok (instance::result, expr_env))
    ([], expr_env) annotations
    >>= fun (instances, expr_env) -> 
    Logger.end_group ();
    Ok (instances, expr_env)

and analize_annotation_inst instance ~none annotation expr_env =
  Logger.info @@ Printf.sprintf "Analizing annotation %s of the instance %s" (string_of_template_annotation annotation |> CString.colorize ~color:Yellow) (List.map string_of_template_inst instance |> String.concat ", " |> CString.colorize ~color:Yellow);
  Logger.debug @@ Printf.sprintf "Expr env: %s" (string_of_env string_of_expr expr_env);
  let module AnnotationEvaluator = MakeAnnotationEvaluator(struct type t = template_instance list end) in
  match annotation with 
  | When expr -> 
    AnnotationEvaluator.when_annotation ~body:instance ~none expr expr_env
  | Foreach (x, expr) -> 
    AnnotationEvaluator.foreach_annotation ~body:instance 
    (fun expr_env x value instances -> 
      map_result
        (fun isnt ->
          Ok (begin_scope expr_env)
          >>= fun expr_env ->
          Ok (bind x.data value expr_env)
          >>= fun expr_env ->
          replace_template_inst isnt expr_env)
        instances)
        x expr expr_env

and deannotate_events events = 
  map_result
    (fun event -> deannotate_event event)
    events

and deannotate_event event =
  Ok { event with data = { event.data with annotations = []} }

and deannotate_template_insts insts = 
  map_result
    (fun inst -> deannotate_template_inst inst)
    insts

and deannotate_template_inst inst =
  Ok { inst with tmpl_annotations = [] }

and deannotate_relations relations = 
  map_result
    (fun relation -> deannotate_relation relation)
    relations

and deannotate_relation relation =
  begin match relation.data with
  | SpawnRelation (from, guard, subprogram, _) -> { relation with data = SpawnRelation (from, guard, subprogram, []) }
  | ControlRelation (from, guard, dest, t, _) -> { relation with data = ControlRelation (from, guard, dest, t, []) }
  end |> Result.ok 

and evaluate_annotations ?(expr_env = empty_env) program  =
  Logger.info "Evaluating annotations";
  (* Evaluate events *)
  let events = program.events in
  let annotation_of_event event = event.data.annotations in
  fold_left_result
    (fun result event -> 
      analize_annotations_of_event [event] ~none:[] (annotation_of_event event) expr_env
      >>= function
      | events::rest -> Ok (List.flatten [result; events; List.flatten rest])
      | [] -> Ok (event::result)
      (* | _ -> failwith "Unsuported annotation for event" *)
      )
    [] events
  >>= deannotate_events
  >>= fun events ->
      (* print_endline @@ Printf.sprintf "Events: %s" (String.concat "\n" (List.map string_of_event events)); *)

  (* Evaluate instantiations *)
  let insts = program.template_insts in
  let annotation_of_inst inst = inst.tmpl_annotations in
  fold_left_result
    (fun (result, expr_env) inst -> 
      analize_annotations_of_inst [inst] ~none:[] (annotation_of_inst inst) expr_env
      >>= fun (insts, expr_env) ->
      match insts with
      | inst::rest -> Ok (List.flatten [result; inst; List.flatten rest], expr_env)
      | [] -> Ok (inst::result, expr_env)
      (* | _ -> failwith "Unsuported annotation for instantiation" *)
      )
    ([], expr_env) insts 
  (* >>= deannotate_template_insts *)
  >>= fun (template_insts, _) ->
  (* print_endline @@ Printf.sprintf "Template insts: %s" (String.concat "\n" (List.map string_of_template_inst template_insts)); *)

  (* Evaluate relations *)
  let relations = program.relations in
  let annotation_of_relation relation = 
    match relation.data with 
    | SpawnRelation (_, _, _, annot) -> annot
    | ControlRelation (_, _, _, _, annot) -> annot in
  fold_left_result
    (fun result relation -> 
      analize_annotations_of_relation [relation] ~none:[] (annotation_of_relation relation) expr_env
      >>= function
      | relation::rest -> Ok (List.flatten [result; relation; List.flatten rest])
      | [] -> Ok (relation::result)
      (* | _ -> failwith "Unsuported annotation for relation" *)
      )
    [] relations
  >>= deannotate_relations
  >>| fun relations ->
  (* Put it together *)
  { program with events ; template_insts; relations }

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
