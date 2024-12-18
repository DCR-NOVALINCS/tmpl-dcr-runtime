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

let rec replace_event ~eval event expr_env =
  update_event_io ~eval event expr_env

and replace_relation ~eval relation (expr_env, event_env, tmpl_env) =
  let replace_id id =
    match find_flat id.data event_env with
    | None -> id_not_found id
    | Some event ->
        let event_id, _ = event.data.info in
        return (event_id, event)
  in
  match relation.data with
  | SpawnRelation (from, guard, subprogram) ->
      replace_id from
      >>= fun (from_id, from_event) ->
      eval guard expr_env
      >>= fun guard ->
      let events, insts, relations, _ = subprogram in
      return (begin_scope expr_env, begin_scope event_env)
      >>= fun (expr_env, event_env) ->
      (* Bind @trigger *)
      return
        ( bind trigger_id (event_as_expr from_event) expr_env
        , bind trigger_id from_event event_env )
      >>= fun (expr_env, event_env) ->
      map (fun event -> replace_event ~eval event expr_env) events
      >>= fun events ->
      map
        (fun inst ->
          replace_template_inst ~eval inst (expr_env, event_env, tmpl_env) )
        insts
      >>= fun insts ->
      map
        (fun relation ->
          replace_relation ~eval relation (expr_env, event_env, tmpl_env) )
        relations
      >>= fun relations ->
      return (events, insts, relations, [])
      >>= fun subprogram ->
      return {relation with data= SpawnRelation (from_id, guard, subprogram)}
  | ControlRelation (from, guard, dest, t) ->
      replace_id from
      >>= fun (from_id, _) ->
      replace_id dest
      >>= fun (dest_id, _) ->
      eval guard expr_env
      >>= fun guard ->
      return {relation with data= ControlRelation (from_id, guard, dest_id, t)}

and replace_template_inst ?(eval = partial_eval_expr) inst
    (expr_env, event_env, tmpl_env) =
  let {args; x= _; tmpl_id; _} = inst.data in
  map
    (fun (arg_id, arg_tmpl_ty) ->
      match arg_tmpl_ty with
      | ExprArg expr ->
          eval expr expr_env >>= fun value -> return (arg_id, ExprArg value)
      | EventArg event_id -> (
        match find_flat event_id.data event_env with
        | None -> event_not_found ~loc:event_id.loc event_id.data
        | Some event ->
            let event_id, _ = event.data.info in
            return (arg_id, EventArg event_id) ) )
    args
  >>= fun args ->
  ( match find_flat tmpl_id.data tmpl_env with
  | None -> tmpl_not_found ~available:(flatten tmpl_env |> List.map fst) tmpl_id
  | Some tmpl -> return tmpl )
  >>= fun _tmpl ->
  (* TODO: "Create" the events that are exported *)
  return {inst with data= {inst.data with args}}

type context = expr env * event env * template_def env

(* =============================================================================
   Binding templates
   ============================================================================= *)

let rec bind_tmpls tmpls (expr_env, event_env, tmpl_env) =
  fold_left
    (fun (expr_env, event_env, tmpl_env) tmpl ->
      bind_tmpl tmpl (expr_env, event_env, tmpl_env) )
    (expr_env, event_env, tmpl_env)
    tmpls

and bind_tmpl tmpl (expr_env, event_env, tmpl_env) =
  let id = tmpl.id in
  Logger.info @@ Printf.sprintf "Binding template %s" (keyword id.data) ;
  return (bind id.data tmpl tmpl_env)
  >>= fun tmpl_env -> return (expr_env, event_env, tmpl_env)

and bind_params params args (expr_env, event_env, tmpl_env) =
  partition_map
    (fun param ->
      let param_id, param_ty = param in
      match List.find_all (fun arg -> (fst arg).data = param_id.data) args with
      (* No occurence of the param *)
      | [] -> (
        match param_ty with
        | ExprParam (_ty, expr_opt) -> (
          match expr_opt with
          | Some expr -> (
              eval_expr expr expr_env
              |> function
              | Ok value -> Either.left (param_id, ExprArg value)
              | _ -> Either.right param )
          | _ -> Either.right param )
        | _ -> Either.right param )
      (* At least one occurence of the param *)
      | arg :: _ -> (
          let _, arg_ty = arg in
          match arg_ty with
          | ExprArg expr -> (
              eval_expr expr expr_env
              |> function
              | Ok value -> Either.left (param_id, ExprArg value)
              | _ -> Either.right param )
          | EventArg event_id -> (
            match find_flat event_id.data event_env with
            | None -> Either.right param
            | Some event ->
                let event_id, _ = event.data.info in
                Either.left (param_id, EventArg event_id) ) ) )
    params
  >>= fun (props, missing) ->
  Logger.debug "Result props length: " ;
  Logger.debug (List.length props |> string_of_int) ;
  Logger.debug "Missing length: " ;
  Logger.debug (List.length missing |> string_of_int) ;
  (* if not (List.length props = List.length params) then invalid_number_of_args
     () else *)
  fold_left
    (fun (expr_env, event_env, tmpl_env) prop ->
      bind_prop prop (expr_env, event_env, tmpl_env) )
    (expr_env, event_env, tmpl_env)
    props

and bind_prop prop (expr_env, event_env, tmpl_env) =
  let prop_id, prop_ty = prop in
  Logger.debug @@ Printf.sprintf "Binding prop: %s" prop_id.data ;
  match prop_ty with
  | ExprArg expr ->
      return (bind prop_id.data expr expr_env)
      >>= fun expr_env -> return (expr_env, event_env, tmpl_env)
  | EventArg event_id -> (
    match find_flat event_id.data event_env with
    | None ->
        Logger.error @@ Printf.sprintf "Event %s not found" event_id.data ;
        event_not_found ~loc:event_id.loc event_id.data
    | Some event ->
        return
          ( bind prop_id.data event event_env
          , bind prop_id.data (event_as_expr event) expr_env )
        >>= fun (event_env, expr_env) -> return (expr_env, event_env, tmpl_env)
    )

(* =============================================================================
   Instantiation
   ============================================================================= *)

and instantiate_tmpls tmpl_insts (expr_env, event_env, tmpl_env) =
  fold_right
    (fun (program, event_env, expr_env) inst ->
      instantiate_tmpl program inst (expr_env, event_env, tmpl_env)
      (* Bind newly instantiated events into the envs *)
      >>= fun (result_program, event_env, expr_env) ->
      let events, _, _, _ = result_program in
      fold_left
        (fun (event_env, expr_env) event ->
          let id, _ = event.data.info in
          return
          @@ ( bind id.data event event_env
             , bind id.data (event_as_expr event) expr_env ) )
        (event_env, expr_env) events
      >>= fun (event_env, expr_env) ->
      return (result_program, event_env, expr_env) )
    (empty_subprogram, event_env, expr_env)
    tmpl_insts

and instantiate_tmpl result_program inst (expr_env, event_env, tmpl_env) =
  let {tmpl_id= id; args; x; _} = inst.data in
  match find_flat id.data tmpl_env with
  | None -> tmpl_not_found ~available:(flatten tmpl_env |> List.map fst) id
  | Some tmpl ->
      Logger.info
      @@ Printf.sprintf "Instantiating template %s" (keyword id.data) ;
      let {export; params; graph; _} = tmpl in
      (* Begin new scope for the template instantiation *)
      return (begin_scope expr_env, begin_scope event_env)
      >>= fun (expr_env, event_env) ->
      preprocess_subprogram ~expr_env ~event_env graph
      >>= fun (event_env, expr_env, graph) ->
      (* Bind params with respective args in the envs *)
      bind_params params args (expr_env, event_env, tmpl_env)
      >>= fun (expr_env, event_env, tmpl_env) ->
      (* DEBUG: Expr_env *)
      Logger.debug @@ "Expr Env before instantiation:" ;
      Logger.debug @@ string_of_env Unparser.PlainUnparser.unparse_expr expr_env ;
      (* DEBUG: Event_env *)
      Logger.debug @@ "Event Env before instantiation:" ;
      Logger.debug
      @@ string_of_env
           (fun e -> Unparser.PlainUnparser.unparse_events [e])
           event_env ;
      (* Get events, instantiations and relations from the template *)
      let events_ti, insts_ti, relations_ti, annotations_ti = graph in
      let result_events, _, result_relations, _ = result_program in
      (* Evaluate template annotations *)
      Logger.debug "Evaluating annotations of the template" ;
      Logger.debug @@ Unparser.PlainUnparser.unparse_annotations annotations_ti ;
      evaluate_annotations annotations_ti (expr_env, event_env, tmpl_env)
      >>= fun ( (annot_events, annot_insts, annot_relations, _)
              , event_env
              , expr_env ) ->
      Logger.debug "Annotations evaluated:" ;
      Logger.debug @@ Unparser.PlainUnparser.unparse_events annot_events ;
      Logger.debug @@ Unparser.PlainUnparser.unparse_relations annot_relations ;
      (* Replace the expression inside of the instantiations of [q_ti] *)
      map
        (fun inst -> instantiate_inst inst (expr_env, event_env, tmpl_env))
        insts_ti
      >>= fun insts_ti ->
      map
        (fun inst -> instantiate_inst inst (expr_env, event_env, tmpl_env))
        annot_insts
      >>= fun annot_insts ->
      (* Instantiate inside instantiations *)
      let insts_ti = List.flatten [insts_ti; annot_insts] in
      instantiate_tmpls insts_ti (expr_env, event_env, tmpl_env)
      >>= fun ((events_q_ti, _, relations_q_ti, _), event_env, expr_env) ->
      (* Instantiate events *)
      map
        (fun event -> instantiate_event ~eval:partial_eval_expr event expr_env)
        events_ti
      >>= fun events_ti ->
      (* Instantiate relations *)
      map
        (fun relation ->
          instantiate_relation ~eval:partial_eval_expr relation
            (expr_env, event_env, tmpl_env) )
        relations_ti
      >>= fun relations_ti ->
      (* Append what got so far *)
      let events_ti = List.flatten [events_q_ti; events_ti; annot_events] in
      let relations_ti =
        List.flatten [relations_q_ti; relations_ti; annot_relations]
      in
      (* Export events *)
      export_map_events x export (events_ti, relations_ti)
      >>= fun (events_ti, relations_ti) ->
      (* Fresh identifiers *)
      fresh_event_ids ~exclude:(deannotate_list x) events_ti relations_ti
      >>= fun (events_ti, relations_ti) ->
      (* Put it all together *)
      Logger.debug
      @@ Printf.sprintf "Instantiated events from template %s:" id.data ;
      Logger.debug @@ Unparser.PlainUnparser.unparse_events events_ti ;
      Logger.debug
      @@ Printf.sprintf "Instantiated relations from template %s:" id.data ;
      Logger.debug @@ Unparser.PlainUnparser.unparse_relations relations_ti ;
      (*Debug envs *)
      Logger.debug @@ "Expr Env after instantiation:" ;
      Logger.debug @@ string_of_env Unparser.PlainUnparser.unparse_expr expr_env ;
      Logger.debug @@ "Event Env after instantiation:" ;
      Logger.debug
      @@ string_of_env
           (fun e -> Unparser.PlainUnparser.unparse_events [e])
           event_env ;
      return
      @@ ( mk_subprogram
             ~events:(List.flatten [events_ti; result_events])
             ~relations:(List.flatten [relations_ti; result_relations])
             ()
         , end_scope event_env
         , end_scope expr_env )

and instantiate_event ~eval target_event expr_env =
  replace_event ~eval target_event expr_env

and instantiate_inst target_inst (expr_env, event_env, tmpl_env) =
  replace_template_inst target_inst (expr_env, event_env, tmpl_env)

and instantiate_relation ~eval target_relation (expr_env, event_env, tmpl_env) =
  replace_relation ~eval target_relation (expr_env, event_env, tmpl_env)

and export_map_events x export (events, relations) =
  if not (List.length x = List.length export) then
    invalid_number_of_exported_events
      ~loc:(append_locs (List.map (fun x -> x.loc) x))
      x export
  else
    let* export_mapping =
      return (List.combine (deannotate_list export) (deannotate_list x))
    in
    let replace_id id =
      match List.assoc_opt id.data export_mapping with
      | None -> return id
      | Some new_id -> return {id with data= new_id}
    in
    map
      (fun event ->
        let id, _ = event.data.info in
        replace_id id >>= fun new_id -> set_info ~id:new_id.data event )
      events
    >>= fun events ->
    map
      (fun relation ->
        match relation.data with
        | SpawnRelation (from, guard, subprogram) ->
            replace_id from
            >>= fun from_id ->
            return
              {relation with data= SpawnRelation (from_id, guard, subprogram)}
        | ControlRelation (from, guard, dest, t) ->
            replace_id from
            >>= fun from_id ->
            replace_id dest
            >>= fun dest_id ->
            return
              {relation with data= ControlRelation (from_id, guard, dest_id, t)}
        )
      relations
    >>= fun relations -> return (events, relations)

(* ==========================================================================
   Annotation Evaluation
   ========================================================================== *)

(* TODO *)

and evaluate_annotations annotations (expr_env, event_env, tmpl_env) =
  fold_right
    (fun (subprograms, event_env, expr_env) annotation ->
      evaluate_annotation annotation (event_env, expr_env, tmpl_env)
      >>= fun (result, event_env, expr_env) ->
      preprocess_subprogram ~expr_env ~event_env result
      >>= fun (event_env, expr_env, result) ->
      return (result :: subprograms, event_env, expr_env) )
    ([], event_env, expr_env) annotations
  >>= fun (subprograms, event_env, expr_env) ->
  append_subprograms subprograms
  >>= fun result -> return (result, event_env, expr_env)

and evaluate_annotation annotation (event_env, expr_env, tmpl_env) =
  let evaluate_subprogram (events, insts, relations, annots)
      (expr_env, event_env, tmpl_env) =
    (* Evaluate annotations in depth *)
    evaluate_annotations annots (expr_env, event_env, tmpl_env)
    >>= fun ( (annot_events, annot_insts, annot_relations, _)
            , event_env
            , expr_env ) ->
    (* Instantiate inner templates *)
    (* let insts = List.flatten [insts; annot_insts] in
       instantiate_tmpls insts (expr_env, event_env, tmpl_env)
       >>= fun ((inst_events, _, inst_relations, _), event_env, expr_env) -> *)
    map (fun event -> instantiate_event ~eval:eval_expr event expr_env) events
    >>= fun events ->
    map
      (fun inst -> instantiate_inst inst (expr_env, event_env, tmpl_env))
      insts
    >>= fun insts ->
    map
      (fun relation ->
        instantiate_relation ~eval:partial_eval_expr relation
          (expr_env, event_env, tmpl_env) )
      relations
    >>= fun relations ->
    let events = List.flatten [events; annot_events] in
    let insts = List.flatten [insts; annot_insts] in
    let relations = List.flatten [relations; annot_relations] in
    return ((events, insts, relations, []), event_env, expr_env)
  in
  match annotation with
  | IfElse {condition; then_branch; else_branch} -> (
      return (begin_scope event_env, begin_scope expr_env)
      >>= fun (branch_event_env, branch_expr_env) ->
      eval_expr condition branch_expr_env
      >>= fun value ->
      (* Debug value *)
      Logger.debug @@ "Value of the if-else annotation:" ;
      Logger.debug @@ Unparser.PlainUnparser.unparse_expr value ;
      match value.data with
      | True ->
          (* Re-evaluate the expressions inside of [body] *)
          evaluate_subprogram then_branch
            (branch_expr_env, branch_event_env, tmpl_env)
          >>= fun (result, _branch_event_env, _branch_expr_env) ->
          preprocess_subprogram ~expr_env ~event_env result
          >>= fun (event_env, expr_env, result) ->
          (* Debug result *)
          Logger.debug "Result of the then branch:" ;
          Logger.debug @@ Unparser.PlainUnparser.unparse_subprogram result ;
          return (result, event_env, expr_env)
      | False -> (
        match else_branch with
        | None -> return (empty_subprogram, event_env, expr_env)
        | Some branch ->
            (* Re-evaluate the expressions inside of [body] *)
            evaluate_subprogram branch
              (branch_expr_env, branch_event_env, tmpl_env)
            >>= fun (result, _branch_event_env, _branch_expr_env) ->
            preprocess_subprogram ~expr_env ~event_env result
            >>= fun (event_env, expr_env, result) ->
            (* Debug result *)
            Logger.debug "Result of the else branch:" ;
            Logger.debug @@ Unparser.PlainUnparser.unparse_subprogram result ;
            return (result, event_env, expr_env) )
      | _ ->
          fixme ~loc:value.loc
            "Expecting a boolean value in the if-else annotation" )
  | Foreach (id, expr, body) ->
      let events, insts, relations, annots = body in
      eval_expr expr expr_env
      >>= fun value ->
      ( match value.data with
      | List elems -> return elems
      | _ -> fixme ~loc:value.loc "Expecting a list in the foreach annotation"
      )
      >>= fun elems ->
      map
        (fun elem ->
          return (begin_scope event_env, begin_scope expr_env)
          >>= fun (foreach_event_env, foreach_expr_env) ->
          return (bind id.data elem foreach_expr_env)
          >>= fun foreach_expr_env ->
          (* Re-evaluate the expressions inside of [body] *)
          evaluate_subprogram
            (events, insts, relations, annots)
            (foreach_expr_env, foreach_event_env, tmpl_env)
          >>= fun ((events, insts, relations, _), _, _) ->
          (* Fresh event'ids *)
          fresh_event_ids events relations
          >>= fun (events, relations) ->
          (* Accumulate the result *)
          return (events, insts, relations, []) )
        elems
      >>= fun subprograms ->
      append_subprograms subprograms
      (* Put new events in the envs *)
      >>= fun result ->
      preprocess_subprogram ~expr_env ~event_env result
      >>= fun (event_env, expr_env, result) ->
      (* Debug result *)
      Logger.debug "Result of the foreach annotation:" ;
      Logger.debug @@ Unparser.PlainUnparser.unparse_subprogram result ;
      return (result, event_env, expr_env)

(* =============================================================================
   Entrypoint
   ============================================================================= *)

let instantiate ?(expr_env = empty_env) ?(event_env = empty_env) program =
  (* Bind all the available templates of the program *)
  let template_decls = program.template_decls in
  bind_tmpls template_decls (expr_env, event_env, empty_env)
  >>= fun (expr_env, event_env, tmpl_env) ->
  (* Evaluate template annotations from [root] program *)
  let annotations = program.annotations in
  evaluate_annotations annotations (expr_env, event_env, tmpl_env)
  >>= fun ((annot_events, annot_insts, annot_relations, _), event_env, expr_env) ->
  (* TODO *)
  (* Instantiate all the instantiations of the program *)
  let insts = List.flatten [program.template_insts; annot_insts] in
  instantiate_tmpls insts (expr_env, event_env, tmpl_env)
  >>= fun ((events, _, relations, _), event_env, expr_env) ->
  (* Append the result in the program *)
  return
    ( { program with
        events= List.flatten [program.events; events; annot_events]
      ; template_insts= []
      ; relations= List.flatten [program.relations; relations; annot_relations]
      ; annotations= [] }
    , event_env
    , expr_env )
