open Misc.Monads.ResultMonad
open Misc.Env
open Misc.Printing
open Syntax
open Evaluation
open Errors
open Program_helper

(* =============================================================================
   Modules & Types
   ============================================================================= *)

module MakeAnnotationEvaluator (T : sig
  type t
end) =
struct
  type body = T.t

  let when_annotation ~(body : body) ~(none : body) expr expr_env =
    eval_expr expr expr_env
    >>= fun value ->
    match value.data with
    | True -> return body
    | False -> return none
    | _ -> invalid_annotation_value value BoolTy

  let foreach_annotation ~(body : body) eval_body x expr expr_env =
    eval_expr expr expr_env
    >>= fun value ->
    match value.data with
    | List values ->
        map (fun value -> eval_body expr_env x value body) values
        >>| List.flatten
    | _ ->
        let ty = match !(expr.ty) with None -> UnitTy | Some ty -> ty in
        invalid_annotation_value value (ListTy (annotate ty))
end

(* =============================================================================
   Binding templates
   ============================================================================= *)

let rec bind_tmpls tmpls env =
  fold_left (fun env tmpl -> bind_tmpl tmpl env) env tmpls

and bind_tmpl tmpl env =
  check_tmpl tmpl env
  >>= fun _ ->
  let id = tmpl.id in
  Logger.info
  @@ Printf.sprintf "Binding template %s"
       (CString.colorize ~color:Yellow id.data) ;
  return (bind id.data tmpl env)

and bind_arg (name, expr) env =
  Logger.debug
  @@ Printf.sprintf "Binding argument %s" (CString.colorize ~color:Yellow name) ;
  eval_expr expr env >>= fun value -> return @@ bind name value env

(* =============================================================================
   Instantiation
   ============================================================================= *)

and check_tmpl tmpl tmpl_env =
  (* Check if the template is duplicated *)
  let id = tmpl.id in
  ( match find_flat id.data tmpl_env with
  | None -> return true
  | Some _ -> duplicate_tmpl id )
  >>= fun _ ->
  (* Check the type of the parameters *)
  iter
    (fun (_, ty, expr_opt) ->
      match expr_opt with
      | None -> return ()
      | Some expr -> (
          eval_expr expr empty_env
          >>= fun value ->
          let value_ty = !(value.ty) in
          match value_ty with
          | None ->
              should_not_happen ~module_path:"instantiation.ml"
                "Type of the value is None"
          | Some ty' ->
              if equal_types ty.data ty' then return ()
              else type_mismatch ~loc:expr.loc [ty.data] [ty'] ) )
    tmpl.params

and check_tmpl_args tmpl_id params args expr_env =
  (* Get available expressions from params (default values) *)
  partition_map
    (fun (param, ty, expr_opt) ->
      let ty' = ty.data in
      match List.find_opt (fun (arg, _) -> arg.data = param.data) args with
      | Some (arg, expr) -> Either.left (arg, ty', expr)
      | None -> (
        match expr_opt with
        | Some expr -> Either.left (param, ty', expr)
        | None -> Either.right (param, ty') ) )
    params
  >>= fun (args, missing_params) ->
  if not (List.length args = List.length params) then
    invalid_number_of_args tmpl_id ~loc:tmpl_id.loc ~missing_params
  else
    map
      (fun (param, ty, expr) ->
        eval_expr expr expr_env
        >>= fun value ->
        let value_ty = !(value.ty) in
        match value_ty with
        | None ->
            should_not_happen ~module_path:"instantiation.ml"
              "Type of the value is None"
        | Some ty' ->
            if equal_types ty ty' then return (param, value)
            else type_mismatch ~loc:expr.loc [ty] [ty'] )
      args

and instantiate_tmpls tmpl_insts tmpl_env expr_env =
  (* Logger.info "Instantiating templates"; *)
  fold_left
    (fun inst_program inst ->
      instantiate_tmpl inst_program inst tmpl_env expr_env )
    empty_subprogram tmpl_insts

and instantiate_tmpl result_program inst tmpl_env expr_env =
  let {tmpl_id= id; args; x; _} = inst.data in
  match find_flat id.data tmpl_env with
  | None -> tmpl_not_found ~available:(flatten tmpl_env |> List.map fst) id
  | Some tmpl ->
      Logger.info
      @@ Printf.sprintf "Instantiating %s"
           (CString.colorize ~color:Yellow id.data) ;
      check_tmpl_args id tmpl.params args expr_env
      >>= fun args ->
      (* Check the number of exported events *)
      ( match List.length x = List.length tmpl.export with
      | true -> return ()
      | false -> invalid_number_of_exported_events x tmpl.export )
      >>= fun _ ->
      let e_ti, q_ti, r_ti = tmpl.graph in
      let result_events, _, result_relations = result_program in
      (* Instantiations should be empty! *)
      (* Begin new scope *)
      return (begin_scope expr_env)
      >>= fun expr_env ->
      (* Bind the arguments to the environment *)
      fold_left
        (fun expr_env event ->
          let id, _ = event.data.info in
          let expr = event_as_expr event in
          bind_arg (id.data, expr) expr_env )
        expr_env e_ti
      >>= fun expr_env ->
      Logger.debug "After binding the events" ;
      Logger.debug @@ string_of_env Unparser.PlainUnparser.unparse_expr expr_env ;
      (* Bind all the arguments to its identifier *)
      (* TODO: Typecheck the arguments *)
      fold_left
        (fun env (prop, expr) -> bind_arg (prop.data, expr) env)
        expr_env args
      >>= fun expr_env ->
      Logger.debug "After binding the args" ;
      Logger.debug @@ string_of_env Unparser.PlainUnparser.unparse_expr expr_env ;
      (* Evaluate the annotations *)
      let program =
        { template_decls= [tmpl]
        ; events= e_ti
        ; template_insts= q_ti
        ; relations= r_ti }
      in
      evaluate_annotations program ~expr_env
      >>= fun {events= e_ti; template_insts= q_ti; relations= r_ti; _} ->
      (* Instantiate events *)
      map (fun e -> instantiate_event expr_env e) e_ti
      >>= fun events ->
      (* Instantations inside of the Template *)
      instantiate_tmpls q_ti tmpl_env expr_env
      >>= fun (other_tmpled_events, _, other_tmpled_relations) ->
      (* Instantiate relations *)
      map (fun r -> instantiate_relation expr_env r) r_ti
      >>= fun relations ->
      (* Fresh ids for the events *)
      let exports_mapping = List.combine (deannotate_list tmpl.export) x in
      fresh_event_ids events relations exports_mapping
      >>| fun (events, relations) ->
      ( List.flatten [result_events; events; other_tmpled_events]
      , []
      , List.flatten [result_relations; relations; other_tmpled_relations] )

and instantiate_event expr_env target_event =
  replace_event target_event expr_env

and instantiate_relation expr_env target_relation =
  replace_relation target_relation expr_env

and replace_event event expr_env = update_event_value event expr_env

and replace_relation relation expr_env =
  (* FIXME: Can't replace the [from] and [dest] ids *)
  match relation.data with
  | SpawnRelation (from, guard, subprogram, annot) ->
      eval_expr guard expr_env
      >>= fun guard ->
      ( match find_flat from.data expr_env with
      | Some {data= Identifier id; _} -> id
      | _ -> from )
      |> fun from ->
      return {relation with data= SpawnRelation (from, guard, subprogram, annot)}
  | ControlRelation (from, guard, dest, t, annot) ->
      eval_expr guard expr_env
      >>= fun guard ->
      Logger.debug @@ Printf.sprintf "From: %s" from.data ;
      Logger.debug @@ string_of_env Unparser.PlainUnparser.unparse_expr expr_env ;
      ( match find_flat from.data expr_env with
      | Some {data= Identifier id; _} -> id
      | Some {data= Record [({data= prop_name; _}, _)]; _}
        when prop_name = "value" ->
          (* This case is weird... *)
          Logger.debug "From is a record with the property value" ;
          from
      | _ ->
          Logger.warn "From not found in the expr_env" ;
          Logger.warn
          @@ Printf.sprintf "Found %s"
               (Unparser.PlainUnparser.unparse_expr
                  (find_flat from.data expr_env |> Option.get) ) ;
          from )
      |> fun from ->
      Logger.debug @@ from.data ;
      ( match find_flat dest.data expr_env with
      | Some {data= Identifier id; _} -> id
      | _ -> dest )
      |> fun dest ->
      Logger.debug @@ Unparser.PlainUnparser.unparse_relations [relation] ;
      Logger.debug
      @@ Unparser.PlainUnparser.unparse_relations
           [{relation with data= ControlRelation (from, guard, dest, t, annot)}] ;
      return {relation with data= ControlRelation (from, guard, dest, t, annot)}

and replace_template_inst inst expr_env =
  let {tmpl_id; args; tmpl_annotations; x} = inst in
  map
    (fun (prop, expr) -> eval_expr expr expr_env >>| fun value -> (prop, value))
    args
  >>= fun args -> return {tmpl_id; args; tmpl_annotations; x}

and export_map_events events export_mapping =
  return @@ List.map (fun event -> map_event_id event export_mapping) events

and map_event_id event export_mapping =
  let id, label = event.data.info in
  match List.assoc_opt id.data export_mapping with
  | None -> event
  | Some new_id -> {event with data= {event.data with info= (new_id, label)}}

(* ==========================================================================
   Annotation Evaluation
   ========================================================================== *)

and analize_annotations_of_event event ~none annotations expr_env =
  (* print_endline "Analizing annotations of events"; *)
  fold_left
    (fun result annotation ->
      analize_annotation_event event ~none annotation expr_env
      >>= fun event -> return (event :: result) )
    [] annotations

and analize_annotation_event event ~none annotation expr_env =
  let module AnnotationEvaluator = MakeAnnotationEvaluator (struct
    type t = event list
  end) in
  match annotation with
  | When expr ->
      AnnotationEvaluator.when_annotation ~body:event ~none expr expr_env
  | Foreach (x, expr) ->
      AnnotationEvaluator.foreach_annotation ~body:event
        (fun expr_env x value body ->
          map
            (fun event ->
              return (begin_scope expr_env)
              >>= fun expr_env ->
              return (bind x.data value expr_env)
              >>= fun expr_env -> replace_event event expr_env )
            body )
        x expr expr_env

and analize_annotations_of_relation relation ~none annotations expr_env =
  (* print_endline "Analizing annotations of relations"; *)
  fold_left
    (fun result annotation ->
      analize_annotation_relation relation ~none annotation expr_env
      >>= fun relation -> return (relation :: result) )
    [] annotations

and analize_annotation_relation relation ~none annotation expr_env =
  let module AnnotationEvaluator = MakeAnnotationEvaluator (struct
    type t = relation list
  end) in
  match annotation with
  | When expr ->
      AnnotationEvaluator.when_annotation ~body:relation ~none expr expr_env
  | Foreach (x, expr) ->
      AnnotationEvaluator.foreach_annotation ~body:relation
        (fun expr_env x value body ->
          map
            (fun relation ->
              return (begin_scope expr_env)
              >>= fun expr_env ->
              return (bind x.data value expr_env)
              >>= fun expr_env -> replace_relation relation expr_env )
            body )
        x expr expr_env

and analize_annotations_of_inst instance ~none annotations expr_env =
  fold_left
    (fun (result, expr_env) annotation ->
      analize_annotation_inst instance ~none annotation expr_env
      >>= fun instance -> return (instance :: result, expr_env) )
    ([], expr_env) annotations
  >>= fun (instances, expr_env) ->
  Logger.end_group () ;
  return (instances, expr_env)

and analize_annotation_inst instance ~none annotation expr_env =
  let module AnnotationEvaluator = MakeAnnotationEvaluator (struct
    type t = template_instance list
  end) in
  match annotation with
  | When expr ->
      AnnotationEvaluator.when_annotation ~body:instance ~none expr expr_env
  | Foreach (_x, _expr) -> todo "Foreach annotation for template instances"
(* AnnotationEvaluator.foreach_annotation ~body:instance (fun expr_env x value
   instances -> map (fun inst -> return (begin_scope expr_env) >>= fun expr_env
   -> return (bind x.data value expr_env) >>= fun expr_env ->
   replace_template_inst inst expr_env ) instances ) x expr expr_env *)

and deannotate_events events = map (fun event -> deannotate_event event) events

and deannotate_event event =
  return {event with data= {event.data with annotations= []}}

and deannotate_template_insts insts =
  map (fun inst -> deannotate_template_inst inst) insts

and deannotate_template_inst inst = return {inst with tmpl_annotations= []}

and deannotate_relations relations =
  map (fun relation -> deannotate_relation relation) relations

and deannotate_relation relation =
  ( match relation.data with
  | SpawnRelation (from, guard, subprogram, _) ->
      {relation with data= SpawnRelation (from, guard, subprogram, [])}
  | ControlRelation (from, guard, dest, t, _) ->
      {relation with data= ControlRelation (from, guard, dest, t, [])} )
  |> return

and evaluate_annotations ?(expr_env = empty_env) program =
  Logger.info "Evaluating annotations" ;
  (* Evaluate events *)
  let events = program.events in
  let annotation_of_event event = event.data.annotations in
  fold_left
    (fun result event ->
      analize_annotations_of_event [event] ~none:[]
        (annotation_of_event event)
        expr_env
      >>= function
      | events :: rest ->
          return (List.flatten [result; events; List.flatten rest])
      | [] -> return (event :: result)
      (* | _ -> failwith "Unsuported annotation for event" *) )
    [] events
  >>= deannotate_events
  >>= fun events ->
  (* print_endline @@ Printf.sprintf "Events: %s" (String.concat "\n" (List.map
     string_of_event events)); *)

  (* Evaluate instantiations *)
  (* let insts = program.template_insts in let annotation_of_inst inst =
     inst.tmpl_annotations in fold_left (fun (result, expr_env) inst ->
     analize_annotations_of_inst [inst] ~none:[] (annotation_of_inst inst)
     expr_env >>= fun (insts, expr_env) -> match insts with | inst :: rest ->
     return (List.flatten [result; inst; List.flatten rest], expr_env) | [] ->
     return (inst :: result, expr_env) ) ([], expr_env) insts *)
  (* >>= deannotate_template_insts *)
  (* >>= fun (template_insts, _) -> *)
  (* print_endline @@ Printf.sprintf "Template insts: %s" (String.concat "\n"
     (List.map string_of_template_inst template_insts)); *)

  (* Evaluate relations *)
  let relations = program.relations in
  let annotation_of_relation relation =
    match relation.data with
    | SpawnRelation (_, _, _, annot) -> annot
    | ControlRelation (_, _, _, _, annot) -> annot
  in
  fold_left
    (fun result relation ->
      analize_annotations_of_relation [relation] ~none:[]
        (annotation_of_relation relation)
        expr_env
      >>= function
      | relation :: rest ->
          return (List.flatten [result; relation; List.flatten rest])
      | [] -> return (relation :: result)
      (* | _ -> failwith "Unsuported annotation for relation" *) )
    [] relations
  >>= deannotate_relations
  >>| fun relations ->
  (* Put it together *)
  {program with events; relations}

(* =============================================================================
   Entrypoint
   ============================================================================= *)

let instantiate ?(expr_env = empty_env) program =
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
  let events, _, relations = tmpled_program in
  return
    ( { program with
        events= List.append program.events events
      ; template_insts= []
      ; relations= List.append program.relations relations }
    , expr_env )
