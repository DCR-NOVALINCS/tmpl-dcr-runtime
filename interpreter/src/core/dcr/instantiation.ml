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
   Modules & Types
   ============================================================================= *)

let rec replace_event ~eval event expr_env =
  update_event_io ~eval event expr_env

and replace_relation ~eval relation (expr_env, event_env, tmpl_env) =
  let replace_id id =
    match find_flat id.data event_env with
    | None ->
        Logger.debug
          (Printf.sprintf "Expr env: %s"
             (string_of_env Plain.unparse_expr expr_env) ) ;
        Logger.debug
          (Printf.sprintf "Event env: %s"
             (string_of_env Plain.unparse_event event_env) ) ;
        event_not_found ~loc:id.loc id.data
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
      let events, insts, relations, annots = subprogram in
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
      map
        (fun annot ->
          replace_annotation ~eval annot (expr_env, event_env, tmpl_env) )
        annots
      >>= fun annots ->
      return (events, insts, relations, annots)
      >>= fun subprogram ->
      return {relation with data= SpawnRelation (from_id, guard, subprogram)}
  | ControlRelation (from, guard, dest, op) ->
      replace_id from
      >>= fun (from_id, _) ->
      replace_id dest
      >>= fun (dest_id, _) ->
      eval guard expr_env
      >>= fun guard ->
      return {relation with data= ControlRelation (from_id, guard, dest_id, op)}

and replace_template_inst ?(eval = partial_eval_expr) inst
    (expr_env, event_env, _tmpl_env) =
  let {args; _} = inst.data in
  (* Replace arguments *)
  map
    (fun (aid, expr) ->
      eval expr expr_env
      >>= fun value ->
      match value.data with
      | EventRef event_ref -> (
          let event = !event_ref in
          let {info= eid, _; _} = event.data in
          match find_flat eid.data event_env with
          | None -> event_not_found ~loc:eid.loc eid.data
          | Some event ->
              event_ref := event ;
              return (aid, {value with data= EventRef event_ref}) )
      | _ -> return (aid, value) )
    args
  >>= fun args ->
  (* map
       (fun (arg_id, arg_tmpl_ty) ->
         match arg_tmpl_ty with
         | ExprArg expr ->
             eval expr expr_env >>= fun value -> return (arg_id, ExprArg value)
         | EventArg event_id -> (
           match find_flat event_id.data event_env with
           | None ->
               Logger.error
               @@ Printf.sprintf
                    "Cannot find event %s to replace in template instantiation"
                    event_id.data ;
               Logger.debug
                 (Printf.sprintf "Event env: %s"
                    (string_of_env Plain.unparse_event event_env) ) ;
               event_not_found ~loc:event_id.loc event_id.data
           | Some event ->
               let event_id, _ = event.data.info in
               return (arg_id, EventArg event_id) ) )
       args
     >>= fun args -> *)
  (* ( match find_flat tmpl_id.data tmpl_env with
     | None -> tmpl_not_found ~available:(flatten tmpl_env |> List.map fst) tmpl_id
     | Some tmpl -> return tmpl )
     >>= fun tmpl ->
     let {graph= events, _, _, _; export; _} = tmpl in
     let export_mapping =
       List.combine (deannotate_list export) (deannotate_list x)
     in
     let* exported_events = filter_map (fun e -> Some e) events in
     let* expr_env = bind_events ~f:event_as_expr exported_events expr_env in
     let* event_env = bind_events ~f:id exported_events event_env in *)
  (* TODO: "Create" the events that are exported *)
  return {inst with data= {inst.data with args}}

and replace_annotation ?(eval = partial_eval_expr) annotation
    (expr_env, event_env, tmpl_env) =
  match annotation with
  | IfElse {condition; then_branch; else_branch} ->
      eval condition expr_env
      >>= fun value ->
      instantiate_subprogram ~eval then_branch ([], [])
        (expr_env, event_env, tmpl_env)
      >>= fun (then_branch, _) ->
      let else_branch = Option.value ~default:empty_subprogram else_branch in
      instantiate_subprogram ~eval else_branch ([], [])
        (expr_env, event_env, tmpl_env)
      >>| fun (else_branch, _) ->
      IfElse {condition= value; then_branch; else_branch= Some else_branch}
  | Foreach (id, expr, body) ->
      eval expr expr_env
      >>= fun value ->
      let expr_env =
        begin_scope expr_env
        |> bind id.data (annotate ~loc:id.loc (Identifier id))
      in
      let event_env = begin_scope event_env in
      Logger.debug
      @@ Printf.sprintf "Expr env:\n%s"
           (string_of_env Colorized.unparse_expr expr_env) ;
      instantiate_subprogram ~eval body ([], []) (expr_env, event_env, tmpl_env)
      >>| fun (body, _) -> Foreach (id, value, body)

(* =============================================================================
   Binding templates
   ============================================================================= *)

and bind_tmpls tmpls (expr_env, event_env, tmpl_env) =
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

and bind_args (args, params) (expr_env, event_env, tmpl_env) =
  let rec bind_id (id, expr) (expr_env, event_env) =
    match expr.data with
    | EventRef event_ref ->
        let event_env = bind id.data !event_ref event_env in
        let expr_env = bind id.data (event_as_expr !event_ref) expr_env in
        return (expr_env, event_env)
    | _ ->
        let expr_env = bind id.data expr expr_env in
        return (expr_env, event_env)
  and bind_param param (expr_env, event_env) =
    let pid, _, default = param in
    match
      (List.find_opt (fun (aid, _) -> aid.data = pid.data) args, default)
    with
    | None, None -> param_not_found pid
    | None, Some expr ->
        eval_expr expr expr_env
        >>= fun value ->
        Logger.info
        @@ Printf.sprintf "Binding default value for %s with %s" pid.data
             (Colorized.unparse_expr value) ;
        bind_id (pid, value) (expr_env, event_env)
    | Some (_, expr), _ ->
        eval_expr expr expr_env
        >>= fun value ->
        Logger.info
        @@ Printf.sprintf "Binding value for %s with %s" pid.data
             (Colorized.unparse_expr value) ;
        bind_id (pid, value) (expr_env, event_env)
  in
  let* expr_env, event_env =
    fold_left
      (fun (expr_env, event_env) param -> bind_param param (expr_env, event_env))
      (expr_env, event_env) params
  in
  Logger.debug
  @@ Printf.sprintf "Bound args in expr env:\n%s"
       (string_of_env Colorized.unparse_expr expr_env) ;
  Logger.debug
  @@ Printf.sprintf "Bound args in event env:\n%s"
       (string_of_env Colorized.unparse_event event_env) ;
  return (expr_env, event_env, tmpl_env)

(* and bind_params params args (expr_env, event_env, tmpl_env) =
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
   if List.length missing > 0 then fixme "error message when missing params"
   else
     fold_left
       (fun (expr_env, event_env, tmpl_env) prop ->
         bind_prop prop (expr_env, event_env, tmpl_env) )
       (expr_env, event_env, tmpl_env)
       props *)

(* and bind_prop prop (expr_env, event_env, tmpl_env) =
   let prop_id, prop_ty = prop in
   Logger.debug @@ Printf.sprintf "Binding prop: %s" prop_id.data ;
   match prop_ty with
   | ExprArg expr ->
       return (bind prop_id.data expr expr_env)
       >>= fun expr_env -> return (expr_env, event_env, tmpl_env)
   | EventArg event_id -> (
     match find_flat event_id.data event_env with
     | None ->
         Logger.error
         @@ Printf.sprintf "Cannot find event %s to bind as parameter"
              event_id.data ;
         event_not_found ~loc:event_id.loc event_id.data
     | Some event ->
         return
           ( bind prop_id.data event event_env
           , bind prop_id.data (event_as_expr event) expr_env )
         >>= fun (event_env, expr_env) -> return (expr_env, event_env, tmpl_env)
     ) *)

(* =============================================================================
   Instantiation
   ============================================================================= *)

and instantiate_tmpls tmpl_insts (expr_env, event_env, tmpl_env) =
  fold_right
    (fun (program, expr_env, event_env) inst ->
      instantiate_tmpl program inst (expr_env, event_env, tmpl_env) )
    (empty_subprogram, expr_env, event_env)
    tmpl_insts

(* and instantiate_tmpl result_program inst (expr_env, event_env, tmpl_env) =
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
       (* Get events, instantiations and relations from the template *)
       let events_ti, insts_ti, relations_ti, annotations_ti = graph in
       let result_events, _, result_relations, _ = result_program in
       (* Evaluate template annotations *)
       Logger.debug "Evaluating annotations of the template" ;
       Logger.debug @@ Plain.unparse_annotations annotations_ti ;
       evaluate_annotations annotations_ti (expr_env, event_env, tmpl_env)
       >>= fun ( (annot_events, annot_insts, annot_relations, _)
               , event_env
               , expr_env ) ->
       Logger.debug "Annotations evaluated:" ;
       Logger.debug @@ Plain.unparse_events annot_events ;
       Logger.debug @@ Plain.unparse_relations annot_relations ;
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
       export_map_events x export (events_ti, relations_ti) (expr_env, event_env)
       >>= fun ((events_ti, relations_ti), (expr_env, event_env)) ->
       (* Fresh identifiers *)
       (* fresh_event_ids ~exclude:(deannotate_list x) events_ti relations_ti
          >>= fun (events_ti, relations_ti) -> *)
       (* Put it all together *)
       return
       @@ ( mk_subprogram
              ~events:(List.flatten [events_ti; result_events])
              ~relations:(List.flatten [relations_ti; result_relations])
              ()
          , deannotate_list x
          , end_scope event_env
          , end_scope expr_env ) *)

and instantiate_tmpl result_program inst (expr_env, event_env, tmpl_env) =
  let {tmpl_id; args; x; _} = inst.data in
  match find_flat tmpl_id.data tmpl_env with
  | None -> tmpl_not_found ~available:(flatten tmpl_env |> List.map fst) tmpl_id
  | Some tmpl ->
      Logger.info (Printf.sprintf "Instantiating %s" (keyword tmpl_id.data)) ;
      let expr_env, event_env = (begin_scope expr_env, begin_scope event_env) in
      let { params
          ; graph= events_ti, insts_ti, relations_ti, annots_ti
          ; export
          ; _ } =
        tmpl
      in
      let* expr_env, event_env, tmpl_env =
        bind_args (args, params) (expr_env, event_env, tmpl_env)
      in
      let* events_ti, relations_ti =
        fresh_event_ids ~exclude:(deannotate_list export) events_ti relations_ti
      in
      let* ( (events_ti, insts_ti, relations_ti, annots_ti)
           , (expr_env, event_env, _tmpl_env) ) =
        instantiate_subprogram ~eval:partial_eval_expr
          (events_ti, insts_ti, relations_ti, annots_ti)
          (export, x)
          (expr_env, event_env, tmpl_env)
      in
      let* inst_result_program, expr_env, event_env =
        instantiate_tmpls insts_ti (expr_env, event_env, tmpl_env)
      in
      (* TODO: evaluate annots *)
      let* annot_result_program, expr_env, event_env =
        instantiate_annotations annots_ti (expr_env, event_env, tmpl_env)
      in
      let expr_env, event_env = (end_scope expr_env, end_scope event_env) in
      (* Put everything together *)
      let* events, insts, relations, annots =
        append_subprograms
          [ result_program
          ; (events_ti, [], relations_ti, [])
          ; inst_result_program
          ; annot_result_program ]
      in
      Logger.success
        (Printf.sprintf "Result inst program of %s:\n%s" (keyword tmpl_id.data)
           (Colorized.unparse_subprogram ~indent:"  "
              (events, insts, relations, annots) ) ) ;
      return ((events, insts, relations, annots), expr_env, event_env)

and instantiate_subprogram ~eval (events, insts, relations, annots) (export, x)
    (expr_env, event_env, tmpl_env) =
  let* events = map (fun e -> instantiate_event ~eval e expr_env) events in
  let* expr_env = bind_events ~f:event_as_expr events expr_env in
  let* event_env = bind_events ~f:id events event_env in
  let* insts =
    map (fun i -> instantiate_inst i (expr_env, event_env, tmpl_env)) insts
  in
  let* (events, insts, relations, annots), (expr_env, event_env, tmpl_env) =
    export_events
      (events, insts, relations, annots)
      (export, x)
      (expr_env, event_env, tmpl_env)
  in
  let* relations =
    map
      (fun r -> instantiate_relation ~eval r (expr_env, event_env, tmpl_env))
      relations
  in
  let* annots =
    map
      (fun a -> instantiate_annot ~eval a (expr_env, event_env, tmpl_env))
      annots
  in
  return ((events, insts, relations, annots), (expr_env, event_env, tmpl_env))

and instantiate_event ~eval target_event expr_env =
  replace_event ~eval target_event expr_env

and instantiate_inst target_inst (expr_env, event_env, tmpl_env) =
  replace_template_inst target_inst (expr_env, event_env, tmpl_env)

and instantiate_relation ~eval target_relation (expr_env, event_env, tmpl_env) =
  replace_relation ~eval target_relation (expr_env, event_env, tmpl_env)

and instantiate_annot ~eval target_annot (expr_env, event_env, tmpl_env) =
  replace_annotation ~eval target_annot (expr_env, event_env, tmpl_env)

and export_events (events, insts, relations, annots) (export, x)
    (expr_env, event_env, tmpl_env) =
  (* let export_mapping =
       List.combine (deannotate_list export) (deannotate_list x)
     in
     let* events =
       map
         (fun e ->
           let {info= eid, _; _} = e.data in
           match List.assoc_opt eid.data export_mapping with
           | None -> return e
           | Some id ->
               Logger.debug
               @@ Printf.sprintf "Exporting event %s as %s" eid.data id ;
               set_info ~id e )
         events
     in
     let* relations =
       let rec replace_id id =
         match List.assoc_opt id.data export_mapping with
         | None -> return id
         | Some nid -> return {id with data= nid}
       and replace_relation r =
         match r.data with
         | ControlRelation (from, guard, dest, t) ->
             replace_id from
             >>= fun from ->
             replace_id dest
             >>= fun dest ->
             return {r with data= ControlRelation (from, guard, dest, t)}
         | SpawnRelation (from, guard, subprogram) ->
             replace_id from
             >>= fun from ->
             let expr_env = begin_scope expr_env in
             let event_env = begin_scope event_env in
             export_events subprogram (export, x) (expr_env, event_env, tmpl_env)
             >>= fun (subprogram, _) ->
             return {r with data= SpawnRelation (from, guard, subprogram)}
       in
       map (fun r -> replace_relation r) relations
     in *)
  let* expr_env, event_env =
    fold_right2
      (fun (expr_env, event_env) exp x ->
        Logger.info
          (Printf.sprintf "Exporting event %s as %s" (keyword exp.data)
             (keyword x.data) ) ;
        match find_flat exp.data event_env with
        | None -> event_not_found ~loc:exp.loc exp.data
        | Some event ->
            let* event = set_info ~id:x.data event in
            return
              ( bind exp.data (event_as_expr event) expr_env
              , bind exp.data event event_env )
        (* return (bind x.data event event_env)
           >>= fun event_env ->
           return (bind x.data (event_as_expr event) expr_env)
           >>= fun expr_env -> return (expr_env, event_env) *) )
      (expr_env, event_env) export x
  in
  Logger.success
    (Printf.sprintf "Exported expr env:\n%s"
       (string_of_env Colorized.unparse_expr expr_env) ) ;
  Logger.success
    (Printf.sprintf "Exported events env:\n%s"
       (string_of_env Colorized.unparse_event event_env) ) ;
  let* events =
    map
      (fun e ->
        replace_event ~eval:partial_eval_expr e expr_env
        >>= fun event ->
        let {info= id, _; _} = event.data in
        match
          List.assoc_opt id.data
            (List.combine (deannotate_list export) (deannotate_list x))
        with
        | None -> return event
        | Some new_id -> set_info ~id:new_id event )
      events
  in
  let* insts =
    map (fun i -> replace_template_inst i (expr_env, event_env, tmpl_env)) insts
  in
  let* relations =
    map
      (fun r ->
        replace_relation ~eval:partial_eval_expr r
          (expr_env, event_env, tmpl_env) )
      relations
  in
  let* annots =
    map
      (fun a ->
        replace_annotation ~eval:partial_eval_expr a
          (expr_env, event_env, tmpl_env) )
      annots
  in
  return ((events, insts, relations, annots), (expr_env, event_env, tmpl_env))

and instantiate_annotations annots (expr_env, event_env, tmpl_env) =
  let instantiate_annotation result_program annot (expr_env, event_env, tmpl_env)
      =
    match annot with
    | Foreach (x, expr, body) -> (
        let instantiate_list item body (expr_env, event_env) =
          let expr_env = begin_scope expr_env |> bind x.data item
          and event_env =
            begin_scope event_env
            |> fun env ->
            match item.data with
            | EventRef event_ref -> bind x.data !event_ref env
            | _ -> env
          in
          Logger.info
          @@ Printf.sprintf "Instantiating list item %s with value %s"
               (keyword x.data)
               (keyword (Colorized.unparse_expr expr)) ;
          let events, insts, relations, annots = body in
          let* events, relations = fresh_event_ids events relations in
          let body = (events, insts, relations, annots) in
          let* expr_env = bind_events ~f:event_as_expr events expr_env in
          let* event_env = bind_events ~f:id events event_env in
          instantiate_subprogram ~eval:partial_eval_expr body ([], [])
            (expr_env, event_env, tmpl_env)
          >>| fun (subprogram, _) -> subprogram
        in
        eval_expr expr expr_env
        >>= fun value ->
        match value.data with
        | List items ->
            map
              (fun item -> instantiate_list item body (expr_env, event_env))
              items
            >>= fun results ->
            append_subprograms (result_program :: results)
            >>= fun result_program ->
            return (result_program, expr_env, event_env)
        | _ ->
            should_not_happen ~module_path:"instantiation.ml"
              "Expected a list in the foreach annotation" )
    | IfElse {condition; then_branch; else_branch} ->
        let instantiate_branch branch (expr_env, event_env) =
          let events, _, _, _ = branch in
          let* expr_env = bind_events ~f:event_as_expr events expr_env in
          let* event_env = bind_events ~f:id events event_env in
          instantiate_subprogram ~eval:partial_eval_expr branch ([], [])
            (expr_env, event_env, tmpl_env)
          >>| fun (subprogram, _) -> subprogram
        in
        eval_expr condition expr_env
        >>= fun value ->
        let expr_env = begin_scope expr_env in
        let event_env = begin_scope event_env in
        ( match value.data with
        | True -> instantiate_branch then_branch (expr_env, event_env)
        | False ->
            let else_branch =
              Option.value ~default:empty_subprogram else_branch
            in
            instantiate_branch else_branch (expr_env, event_env)
        | _ ->
            should_not_happen ~module_path:"instantiation.ml"
              "Expected a boolean value in the if-else annotation" )
        >>= fun subprogram ->
        append_subprograms [result_program; subprogram]
        >>= fun result_program -> return (result_program, expr_env, event_env)
  in
  fold_right
    (fun (result, expr_env, event_env) annot ->
      instantiate_annotation result annot (expr_env, event_env, tmpl_env)
      (* >>= fun (result_program, expr_env, event_env) ->
         append_subprograms [result; result_program]
         >>= fun result ->
         return (result, expr_env, event_env) *) )
    (empty_subprogram, expr_env, event_env)
    annots
  >>= fun (result, expr_env, event_env) ->
  instantiate_sub result (expr_env, event_env, tmpl_env)
  >>= fun (result, expr_env, event_env) ->
  Logger.success
    (Printf.sprintf "Result of the annotation:\n%s"
       (Colorized.unparse_subprogram ~indent:"  " result) ) ;
  return (result, expr_env, event_env)

(* and export_map_events x export (events, relations) (expr_env, event_env) =
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
     >>= fun relations ->
     bind_events ~f:id events event_env
     >>= fun event_env ->
     bind_events ~f:event_as_expr events expr_env
     >>= fun expr_env -> return ((events, relations), (expr_env, event_env)) *)

(* ==========================================================================
   Annotation Evaluation
   ========================================================================== *)

(* TODO *)

(* and evaluate_annotations annotations (expr_env, event_env, tmpl_env) =
     fold_right
       (fun (subprograms, event_env, expr_env) annotation ->
         Logger.debug "Evaluating annotation" ;
         Logger.debug @@ Plain.unparse_annotation annotation ;
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
       (* Instantiate inner templates *)
       (* let insts = List.flatten [insts; ] in *)
       instantiate_tmpls insts (expr_env, event_env, tmpl_env)
       >>= fun ((inst_events, _, inst_relations, _), expr_env, event_env) ->
       let events = List.append events inst_events in
       let relations = List.append relations inst_relations in
       (* Evaluate annotations in depth *)
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
       (* Evaluate annotations in depth *)
       evaluate_annotations annots (expr_env, event_env, tmpl_env)
       >>= fun ( (annot_events, annot_insts, annot_relations, _)
               , event_env
               , expr_env ) ->
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
         Logger.debug @@ Plain.unparse_expr value ;
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
             Logger.debug @@ Plain.unparse_subprogram result ;
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
               Logger.debug @@ Plain.unparse_subprogram result ;
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
             >>= fun ((events, insts, relations, annotations), _, _) ->
             (* Fresh event'ids *)
             fresh_event_ids events relations
             >>= fun (events, relations) ->
             (* Accumulate the result *)
             return (events, insts, relations, annotations) )
           elems
         >>= fun subprograms ->
         append_subprograms subprograms
         (* Put new events in the envs *)
         >>= fun result ->
         preprocess_subprogram ~expr_env ~event_env result
         >>= fun (event_env, expr_env, result) ->
         (* Debug result *)
         Logger.debug "Result of the foreach annotation:" ;
         Logger.debug @@ Plain.unparse_subprogram result ;
         return (result, event_env, expr_env) *)

(* =============================================================================
   Entrypoint
   ============================================================================= *)

and instantiate_sub (events, insts, relations, annots)
    (expr_env, event_env, tmpl_env) =
  ( if List.length insts = 0 then return (empty_subprogram, expr_env, event_env)
    else instantiate_tmpls insts (expr_env, event_env, tmpl_env) )
  >>= fun ((inst_events, _, inst_relations, inst_annots), expr_env, event_env) ->
  (* Evaluate template annotations from the program *)
  let annots = List.append annots inst_annots in
  ( if List.length annots = 0 then return (empty_subprogram, expr_env, event_env)
    else instantiate_annotations annots (expr_env, event_env, tmpl_env) )
  >>= fun (annot_program, expr_env, event_env) ->
  (* Append the result in the program and envs *)
  let* events, _, relations, _ =
    append_subprograms
      [ (events, [], relations, [])
      ; (inst_events, [], inst_relations, [])
      ; annot_program ]
  in
  let* event_env = bind_events ~f:id events event_env in
  let* expr_env = bind_events ~f:event_as_expr events expr_env in
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
  assert (List.length insts = 0 && List.length annots = 0) ;
  return
    ( {program with events; template_insts= []; relations; annotations= []}
    , event_env
    , expr_env )
