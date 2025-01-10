open Helper
open Errors
open Evaluation
open Ast
open Syntax
open Error
open Unparser
open Common
open Monads.ResultMonad
open Env
open Checkable
open Printing

(* =============================================================================
   Binding templates and arguments
   ============================================================================= *)

let rec bind_tmpls tmpls (expr_env, event_env, tmpl_env) =
  let bind tmpl (expr_env, event_env, tmpl_env) =
    let id = tmpl.id in
    Logger.info @@ Printf.sprintf "Binding template %s" (keyword id.data) ;
    return (bind id.data tmpl tmpl_env)
    >>| fun tmpl_env -> (expr_env, event_env, tmpl_env)
  in
  fold_left
    (fun (expr_env, event_env, tmpl_env) tmpl ->
      bind tmpl (expr_env, event_env, tmpl_env) )
    (expr_env, event_env, tmpl_env)
    tmpls

and bind_args (args, params) ?(eval = partial_eval_expr)
    (expr_env, event_env, tmpl_env) =
  let rec bind_arg (id, expr) (expr_env, event_env) =
    eval expr expr_env
    >>= fun value ->
    match value.data with
    | EventRef event_ref ->
        let event_env = bind id.data !event_ref event_env in
        let expr_env = bind id.data value expr_env in
        return (expr_env, event_env)
    | _ ->
        let expr_env = bind id.data expr expr_env in
        return (expr_env, event_env)
  and bind_param param (expr_env, event_env) =
    let pid, _, default = param in
    match (find_flat pid.data expr_env, default) with
    | None, None -> param_not_found pid
    | None, Some expr ->
        Logger.info
        @@ Printf.sprintf "Binding default value for %s with %s"
             (keyword pid.data)
             (Colorized.unparse_expr expr) ;
        let* value = eval expr expr_env in
        bind_arg (pid, value) (expr_env, event_env)
    | _ -> return (expr_env, event_env)
  in
  fold_left
    (fun (expr_env, event_env) arg -> bind_arg arg (expr_env, event_env))
    (expr_env, event_env) args
  >>= fun (expr_env, event_env) ->
  fold_left
    (fun (expr_env, event_env) param -> bind_param param (expr_env, event_env))
    (expr_env, event_env) params
  >>= fun (expr_env, event_env) ->
  Logger.debug
  @@ Printf.sprintf "Bound args in expr env:\n%s"
       (string_of_env Colorized.unparse_expr expr_env) ;
  Logger.debug
  @@ Printf.sprintf "Bound args in event env:\n%s"
       (string_of_env Colorized.unparse_event event_env) ;
  return (expr_env, event_env, tmpl_env)

(* =============================================================================
   Instantiation
   ============================================================================= *)

and instantiate_insts insts ?(eval = partial_eval_expr)
    (expr_env, event_env, tmpl_env) =
  let instantiate_inst inst program (expr_env, event_env, tmpl_env) =
    let {tmpl_id; x; args; _} = inst.data in
    match find_flat tmpl_id.data tmpl_env with
    | None ->
        tmpl_not_found ~available:(flatten tmpl_env |> List.map fst) tmpl_id
    | Some tmpl ->
        Logger.info (Printf.sprintf "Instantiating %s" (keyword tmpl_id.data)) ;
        let expr_env, event_env =
          (begin_scope expr_env, begin_scope event_env)
        in
        let { params
            ; export
            ; graph= events_ti, insts_ti, relations_ti, annots_ti
            ; _ } =
          tmpl
        in
        (* Bind the arguments of the template *)
        let* expr_env, event_env, tmpl_env =
          bind_args (args, params) (expr_env, event_env, tmpl_env)
        in
        let export' = deannotate_list export in
        (* Rename exported events, affecting the whole program *)
        let export_mapping = List.combine export' x in
        let* events_ti, insts_ti, relations_ti, annots_ti =
          update_ids export_mapping
            (events_ti, insts_ti, relations_ti, annots_ti)
        in
        (* Bind the events in the env *)
        let* exported_events, other_events =
          partition_map
            (fun e ->
              let {info= id, label; _} = e.data in
              match List.assoc_opt id.data export_mapping with
              | None -> Right e
              | Some new_id ->
                  let info = (new_id, label) in
                  Left {e with data= {e.data with info}} )
            events_ti
        in
        Logger.success
        @@ Printf.sprintf "Exported events:\n%s"
             (Colorized.unparse_events exported_events) ;
        Logger.success
        @@ Printf.sprintf "Other events:\n%s"
             (Colorized.unparse_events other_events) ;
        let* expr_env, event_env =
          fold_right
            (fun (expr_env, event_env) event ->
              let {info= id, _; _} = event.data in
              (* Assuming that when instantiating a template, opens a new scope *)
              return
                ( bind_at_depth id.data (event_as_expr event) 1 expr_env
                , bind_at_depth id.data event 1 event_env ) )
            (expr_env, event_env) exported_events
          >>= fun (expr_env, event_env) ->
          fold_right
            (fun (expr_env, event_env) event ->
              let {info= id, _; _} = event.data in
              return
                ( bind id.data (event_as_expr event) expr_env
                , bind id.data event event_env ) )
            (expr_env, event_env) other_events
        in
        Logger.success
        @@ Printf.sprintf "Bound events in expr env:\n%s"
             (string_of_env Colorized.unparse_expr expr_env) ;
        Logger.success
        @@ Printf.sprintf "Bound events in event env:\n%s"
             (string_of_env Colorized.unparse_event event_env) ;
        (* Instantiate inner templates *)
        let* inst_program, (expr_env, event_env, tmpl_env) =
          instantiate_insts insts_ti (expr_env, event_env, tmpl_env)
        in
        (* Instantiate inner annotations *)
        (* let* annot_program, (expr_env, event_env, tmpl_env) =
             instantiate_annotations annots_ti (expr_env, event_env, tmpl_env)
           in *)
        (* Partially Evaluate subprogram *)
        let events_ti = List.append exported_events other_events in
        let* ( (events_ti, _, relations_ti, annots_ti)
             , (expr_env, event_env, tmpl_env) ) =
          evaluate_subprogram ~eval
            (events_ti, [], relations_ti, annots_ti)
            (expr_env, event_env, tmpl_env)
        in
        (* Fresh event identifiers *)
        let* result =
          fresh_event_ids ~exclude:(deannotate_list x)
            (events_ti, [], relations_ti, annots_ti)
        in
        (* Append all results *)
        let* program =
          append_subprograms [program; result; inst_program (* annot_program *)]
        in
        let program_events, _, _, _ = program in
        Logger.success
        @@ Printf.sprintf "Result program after instantiating %s:\n%s"
             (keyword tmpl_id.data)
             (Colorized.unparse_subprogram program) ;
        let expr_env, event_env = (end_scope expr_env, end_scope event_env) in
        let* expr_env = bind_events ~f:event_as_expr program_events expr_env in
        let* event_env = bind_events ~f:id program_events event_env in
        return (program, (expr_env, event_env, tmpl_env))
  in
  fold_right
    (fun (result, (expr_env, event_env, tmpl_env)) inst ->
      instantiate_inst inst result (expr_env, event_env, tmpl_env) )
    (empty_subprogram, (expr_env, event_env, tmpl_env))
    insts

and evaluate_subprogram ?(eval = partial_eval_expr)
    (events, insts, relations, annots) (expr_env, event_env, tmpl_env) =
  let evaluate_event event =
    (* Update IO *)
    update_event_io event expr_env
  and evaluate_inst inst =
    let {args; _} = inst.data
    (* Update arguments *)
    and update_arg arg =
      let id, expr = arg in
      let* value = eval expr expr_env in
      match value.data with
      | EventRef event_ref -> (
          let event = !event_ref in
          let {info= eid, _; _} = event.data in
          match find_flat eid.data event_env with
          | None -> event_not_found ~loc:eid.loc eid.data
          | Some event ->
              event_ref := event ;
              return (id, {value with data= EventRef event_ref}) )
      | _ -> return (id, value)
    in
    let* args = map update_arg args in
    return {inst with data= {inst.data with args}}
  and evaluate_relation relation =
    let replace_id id =
      match find_flat id.data event_env with
      | None -> event_not_found ~loc:id.loc id.data
      | Some event ->
          let event_id, _ = event.data.info in
          return (event_id, event)
    in
    match relation.data with
    | ControlRelation (from, guard, dest, op) ->
        let* from, _ = replace_id from in
        let* guard = eval guard expr_env in
        let* dest, _ = replace_id dest in
        return {relation with data= ControlRelation (from, guard, dest, op)}
    | SpawnRelation (from, guard, subprogram) ->
        let expr_env, event_env, tmpl_env =
          (begin_scope expr_env, begin_scope event_env, tmpl_env)
        in
        let* from, from_event = replace_id from in
        let* guard = eval guard expr_env in
        let* expr_env, event_env =
          return
            ( bind trigger_id (annotate (Identifier from)) expr_env
            , bind trigger_id from_event event_env )
        in
        let* subprogram, _ =
          evaluate_subprogram ~eval:partial_eval_expr subprogram
            (expr_env, event_env, tmpl_env)
        in
        return {relation with data= SpawnRelation (from, guard, subprogram)}
  and evaluate_annot annot =
    let expr_env, event_env, tmpl_env =
      (begin_scope expr_env, begin_scope event_env, tmpl_env)
    in
    match annot with
    | IfElse {condition; then_branch; else_branch} ->
        let* condition = eval condition expr_env in
        let* then_branch, _ =
          evaluate_subprogram ~eval then_branch (expr_env, event_env, tmpl_env)
        in
        let* else_branch =
          match else_branch with
          | None -> return None
          | Some else_branch ->
              let* else_branch, _ =
                evaluate_subprogram ~eval else_branch
                  (expr_env, event_env, tmpl_env)
              in
              return (Some else_branch)
        in
        return (IfElse {condition; then_branch; else_branch})
    | Foreach (id, expr, body) ->
        let* value = eval expr expr_env in
        let expr_env = bind id.data (annotate (Identifier id)) expr_env in
        let* body, _ =
          evaluate_subprogram ~eval body (expr_env, event_env, tmpl_env)
        in
        return (Foreach (id, value, body))
  in
  let* events = map evaluate_event events in
  let* insts = map evaluate_inst insts in
  let* relations = map evaluate_relation relations in
  let* annots = map evaluate_annot annots in
  return ((events, insts, relations, annots), (expr_env, event_env, tmpl_env))

(* ==========================================================================
   Annotation Evaluation
   ========================================================================== *)

and instantiate_annotations annots ?(eval = partial_eval_expr)
    (expr_env, event_env, tmpl_env) =
  let evaluate_annotation_body body (expr_env, event_env, tmpl_env) =
    let events, insts, relations, annots = body in
    (* Instantiate inner templates *)
    let* inst_program, (expr_env, event_env, tmpl_env) =
      instantiate_insts ~eval:partial_eval_expr insts
        (expr_env, event_env, tmpl_env)
    in
    (* Instantiate inner annotations *)
    let* annot_program, (expr_env, event_env, tmpl_env) =
      instantiate_annotations ~eval annots (expr_env, event_env, tmpl_env)
    in
    (* Partially Evaluate subprogram *)
    let* (events, _, relations, _), _ =
      evaluate_subprogram
        (events, [], relations, [])
        (expr_env, event_env, tmpl_env)
    in
    (* Fresh event identifiers *)
    let* body = fresh_event_ids (events, [], relations, []) in
    let* body = append_subprograms [body; inst_program; annot_program] in
    return body
  in
  let instantiate_annotation result annot (expr_env, event_env, tmpl_env) =
    let expr_env, event_env, tmpl_env =
      (begin_scope expr_env, begin_scope event_env, tmpl_env)
    in
    ( match annot with
    | IfElse {condition; then_branch; else_branch} ->
        let* is_true = eval_bool condition expr_env in
        let branch =
          if is_true then then_branch
          else Option.value ~default:empty_subprogram else_branch
        in
        evaluate_annotation_body branch (expr_env, event_env, tmpl_env)
    | Foreach (id, expr, body) ->
        let* value = eval_expr expr expr_env in
        ( match value.data with
        | List items ->
            map
              (fun item ->
                let* item = eval_expr item expr_env in
                let expr_env = bind id.data item expr_env in
                let event_env =
                  match item.data with
                  | EventRef event_ref -> bind id.data !event_ref event_env
                  | _ -> event_env
                in
                Logger.debug
                @@ Printf.sprintf "Instantiating list item %s with %s"
                     (keyword id.data)
                     (Colorized.unparse_expr item) ;
                Logger.success
                  (Printf.sprintf "Subprogram:\n%s"
                     (Colorized.unparse_subprogram body) ) ;
                let events, insts, relations, annots = body in
                (* Instantiate inner templates *)
                let* inst_program, (expr_env, event_env, tmpl_env) =
                  instantiate_insts insts (expr_env, event_env, tmpl_env)
                in
                (* Instantiate inner annotations *)
                let* annot_program, (expr_env, event_env, tmpl_env) =
                  instantiate_annotations annots (expr_env, event_env, tmpl_env)
                in
                (* Partially Evaluate subprogram *)
                let* (events, _, relations, _), _ =
                  evaluate_subprogram
                    (events, [], relations, [])
                    (expr_env, event_env, tmpl_env)
                in
                (* Fresh event identifiers *)
                let* body = fresh_event_ids (events, [], relations, []) in
                let* body =
                  append_subprograms [body; inst_program; annot_program]
                in
                return body )
              items
        | _ ->
            should_not_happen ~module_path:"instantiation.ml"
              "Expected a list in the foreach annotation" )
        >>= fun programs -> append_subprograms programs )
    >>= fun annot_result ->
    let* result = append_subprograms [result; annot_result] in
    Logger.success
    @@ Printf.sprintf "Result after evaluating annotation '%s':\n%s"
         (Colorized.unparse_annotation annot)
         (Colorized.unparse_subprogram ~indent:"  " result) ;
    return (result, (end_scope expr_env, end_scope event_env, tmpl_env))
  in
  fold_right
    (fun (result, (expr_env, event_env, tmpl_env)) annot ->
      instantiate_annotation result annot (expr_env, event_env, tmpl_env) )
    (empty_subprogram, (expr_env, event_env, tmpl_env))
    annots

(* =============================================================================
   Entrypoint
   ============================================================================= *)

and instantiate_sub (events, insts, relations, annots)
    (expr_env, event_env, tmpl_env) =
  ( if List.length insts = 0 then
      return (empty_subprogram, (expr_env, event_env, tmpl_env))
    else instantiate_insts insts (expr_env, event_env, tmpl_env) )
  >>= fun ( (inst_events, _, inst_relations, inst_annots)
          , (expr_env, event_env, tmpl_env) ) ->
  (* Evaluate template annotations from the program *)
  let annots = List.append annots inst_annots in
  ( if List.length annots = 0 then
      return (empty_subprogram, (expr_env, event_env, tmpl_env))
    else
      instantiate_annotations ~eval:partial_eval_expr annots
        (expr_env, event_env, tmpl_env) )
  >>= fun (annot_program, (expr_env, event_env, _tmpl_env)) ->
  (* Append the result in the program and envs *)
  let* events, _, relations, _ =
    append_subprograms
      [ (events, [], relations, [])
      ; (inst_events, [], inst_relations, [])
      ; annot_program ]
  in
  (* let* event_env = bind_events ~f:id events event_env in
     let* expr_env = bind_events ~f:event_as_expr events expr_env in *)
  return ((events, [], relations, []), expr_env, event_env)

and instantiate ?(expr_env = empty_env) ?(event_env = empty_env) program =
  (* Bind all the available templates of the program *)
  let template_decls = program.template_decls in
  bind_tmpls template_decls (expr_env, event_env, empty_env)
  >>= fun (expr_env, event_env, tmpl_env) ->
  (* Instantiate all templates of the program *)
  let program' = to_subprogram program in
  instantiate_sub program' (expr_env, event_env, tmpl_env)
  >>= fun ((events, insts, relations, annots), expr_env, event_env) ->
  (* Reassuring that there is no instantiation/annotation evaluation to do. *)
  assert (List.length insts = 0 && List.length annots = 0) ;
  return
    ( {program with events; template_insts= []; relations; annotations= []}
    , event_env
    , expr_env )
