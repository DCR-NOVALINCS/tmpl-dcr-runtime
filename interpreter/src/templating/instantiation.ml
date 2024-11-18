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

module EventAnnotationEvaluator = struct
  type t = event list

  let when_annotation ~body ~none expr (expr_env, _, _) =
    eval_expr expr expr_env
    >>= fun value ->
    match value.data with
    | True -> return body
    | False -> return none
    | _ -> type_mismatch [BoolTy] []

  let foreach_annotation ~body id expr (expr_env, _, _) =
    eval_expr expr expr_env
    >>= fun value ->
    match value.data with
    | List lst ->
        fold_left
          (fun events item_expr ->
            eval_expr item_expr expr_env
            >>= fun item_value ->
            return (bind id.data item_value expr_env)
            >>= fun expr_env ->
            map (fun event -> update_event_value event expr_env) events )
          body lst
    | _ -> type_mismatch [ListTy (annotate UnitTy)] []
end

type context = expr env * event env * template_def env

(* =============================================================================
   Helper functions
   ============================================================================= *)

(* =============================================================================
   Error handling
   ============================================================================= *)

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
  Logger.info
  @@ Printf.sprintf "Binding template %s"
       (CString.colorize ~color:Yellow id.data) ;
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
  return (begin_scope expr_env)
  >>= fun expr_env ->
  return (begin_scope event_env)
  >>= fun event_env ->
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
    | None -> event_not_found event_id.data
    | Some event ->
        return (bind prop_id.data event event_env)
        >>= fun event_env -> return (expr_env, event_env, tmpl_env) )

(* =============================================================================
   Instantiation
   ============================================================================= *)

and instantiate_tmpls tmpl_insts (expr_env, event_env, tmpl_env) =
  fold_left
    (fun program inst ->
      instantiate_tmpl program inst (expr_env, event_env, tmpl_env) )
    empty_subprogram tmpl_insts

and instantiate_tmpl result_program inst (expr_env, event_env, tmpl_env) =
  let {tmpl_id= id; args; x; _} = inst.data in
  match find_flat id.data tmpl_env with
  | None -> tmpl_not_found ~available:(flatten tmpl_env |> List.map fst) id
  | Some tmpl ->
      let {export; params; export_types= _; graph; _} = tmpl in
      (* Bind params with respective args in the envs *)
      bind_params params args (expr_env, event_env, tmpl_env)
      >>= fun (expr_env, event_env, _tmpl_env) ->
      (* Get events, instantiations and relations from the template *)
      let events_ti, insts_ti, relations_ti = graph in
      let result_events, _, result_relations = result_program in
      (* Replace the expression inside of the instantiations of [q_ti] *)
      map (fun inst -> instantiate_inst inst (expr_env, event_env)) insts_ti
      >>= fun insts_ti ->
      (* Instantiate inside instantiations *)
      instantiate_tmpls insts_ti (expr_env, event_env, tmpl_env)
      >>= fun (events_q_ti, _, relations_q_ti) ->
      (* Instantiate events *)
      map (fun event -> instantiate_event event expr_env) events_ti
      >>= fun events_ti ->
      (* Instantiate relations *)
      map
        (fun relation -> instantiate_relation relation (expr_env, event_env))
        relations_ti
      >>= fun relations_ti ->
      (* Append what got so far *)
      let events_ti = List.flatten [events_q_ti; events_ti] in
      let relations_ti = List.flatten [relations_q_ti; relations_ti] in
      (* Export events *)
      export_map_events x export (events_ti, relations_ti)
      >>= fun (events_ti, relations_ti) ->
      (* Fresh identifiers *)
      fresh_event_ids events_ti relations_ti []
      >>= fun (events_ti, relations_ti) ->
      (* Put it all together *)
      return
      @@ mk_subprogram
           ~events:(List.flatten [events_ti; result_events])
           ~relations:(List.flatten [relations_ti; result_relations])
           ()

and instantiate_event target_event expr_env =
  replace_event target_event expr_env

and instantiate_inst target_inst (expr_env, event_env) =
  replace_template_inst target_inst (expr_env, event_env)

and instantiate_relation target_relation (expr_env, event_env) =
  replace_relation target_relation (expr_env, event_env)

and replace_event event expr_env = update_event_value event expr_env

and replace_relation relation (expr_env, event_env) =
  let replace_id id =
    match find_flat id.data event_env with
    | None -> return id
    | Some event ->
        let event_id, _ = event.data.info in
        return event_id
  in
  match relation.data with
  | SpawnRelation (from, guard, subprogram, annotations) ->
      replace_id from
      >>= fun from ->
      eval_expr guard expr_env
      >>= fun guard ->
      return
        { relation with
          data= SpawnRelation (from, guard, subprogram, annotations) }
  | ControlRelation (from, guard, dest, t, annotations) ->
      replace_id from
      >>= fun from ->
      replace_id dest
      >>= fun dest ->
      eval_expr guard expr_env
      >>= fun guard ->
      return
        {relation with data= ControlRelation (from, guard, dest, t, annotations)}

and replace_template_inst inst (expr_env, event_env) =
  let {args; _} = inst.data in
  Logger.debug "Args: " ;
  Logger.debug "Expr based args: " ;
  Logger.debug
  @@ ( List.map
         (fun (id, _) -> id.data)
         (List.filter
            (fun (_, arg_ty) ->
              match arg_ty with ExprArg _ -> true | _ -> false )
            args )
     |> String.concat ", " ) ;
  Logger.debug "Event based args: " ;
  Logger.debug
  @@ ( List.map
         (fun (id, _) -> id.data)
         (List.filter
            (fun (_, arg_ty) ->
              match arg_ty with EventArg _ -> true | _ -> false )
            args )
     |> String.concat ", " ) ;
  Logger.debug "Expr env: " ;
  Logger.debug @@ string_of_env Unparser.PlainUnparser.unparse_expr expr_env ;
  Logger.debug "Event env: " ;
  Logger.debug
  @@ string_of_env
       (fun event -> Unparser.PlainUnparser.unparse_events [event])
       event_env ;
  map
    (fun (arg_id, arg_tmpl_ty) ->
      match arg_tmpl_ty with
      | ExprArg expr ->
          eval_expr expr expr_env >>= fun value -> return (arg_id, ExprArg value)
      | EventArg event_id -> (
        match find_flat event_id.data event_env with
        | None -> event_not_found event_id.data
        | Some event ->
            let event_id, _ = event.data.info in
            return (arg_id, EventArg event_id) ) )
    args
  >>= fun args -> return {inst with data= {inst.data with args}}

and export_map_events x export (events, relations) =
  if not (List.length x = List.length export) then
    invalid_number_of_exported_events x export
  else if List.length x > List.length events then fixme ""
  else
    return (List.combine (deannotate_list x) (deannotate_list export))
    >>= fun export_mapping ->
    map
      (fun event ->
        let id, label = event.data.info in
        match List.assoc_opt id.data export_mapping with
        | None -> return event
        | Some new_id ->
            return
              { event with
                data=
                  {event.data with info= (annotate ~loc:id.loc new_id, label)}
              } )
      events
    >>= fun events ->
    map
      (fun relation ->
        let replace_id id =
          match List.assoc_opt id.data export_mapping with
          | None -> return id
          | Some new_id -> return @@ annotate ~loc:id.loc new_id
        in
        match relation.data with
        | SpawnRelation (from, guard, subprogram, annotations) ->
            replace_id from
            >>= fun from_id ->
            return
              { relation with
                data= SpawnRelation (from_id, guard, subprogram, annotations) }
        | ControlRelation (from, guard, dest, t, annotations) ->
            replace_id from
            >>= fun from_id ->
            replace_id dest
            >>= fun dest_id ->
            return
              { relation with
                data= ControlRelation (from_id, guard, dest_id, t, annotations)
              } )
      relations
    >>= fun relations -> return (events, relations)

(* ==========================================================================
   Annotation Evaluation
   ========================================================================== *)

and evaluate_event_annotation event (expr_env, event_env, tmpl_env) =
  let {annotations; _} = event.data in
  map
    (fun annotation ->
      match annotation with
      | When expr ->
          EventAnnotationEvaluator.when_annotation ~body:[event] ~none:[] expr
            (expr_env, event_env, tmpl_env)
      | Foreach (id, expr) ->
          EventAnnotationEvaluator.foreach_annotation ~body:[event] id expr
            (expr_env, event_env, tmpl_env) )
    annotations

(* =============================================================================
   Entrypoint
   ============================================================================= *)

let instantiate ?(expr_env = empty_env) ?(event_env = empty_env) program =
  (* Bind all the available templates of the program *)
  let template_decls = program.template_decls in
  bind_tmpls template_decls (expr_env, event_env, empty_env)
  >>= fun (expr_env, event_env, tmpl_env) ->
  (* Instantiate all the instantiations of the program *)
  let insts = program.template_insts in
  instantiate_tmpls insts (expr_env, event_env, tmpl_env)
  >>= fun tmpled_program ->
  (* Append the result in the program *)
  let events, _, relations = tmpled_program in
  return
    ( { program with
        events= List.append program.events events
      ; template_insts= []
      ; relations= List.append program.relations relations }
    , expr_env )
