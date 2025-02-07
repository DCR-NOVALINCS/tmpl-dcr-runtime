open Ast.Syntax
open Unparser

let event_as_ty event =
  let {io= _; info= _, label; _} = event.data in
  (* let ty =
       match io.data with
       | Input ty -> ty.data
       | Output expr -> (
         match !(expr.ty) with None -> failwith "Type not found" | Some ty -> ty )
     in *)
  (* RecordTy [(annotate "value", annotate ty)] *)
  EventTy label

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Aux functions, types and modules                                         │
   └──────────────────────────────────────────────────────────────────────────┘ *)

type event_kind = type_expr' * event_type'

type event_type_value = Undefined | Defined of event_kind

module EventTypes = struct
  (* type label_type = {label: string; kind: event_kind}

     let mk_label_type label kind = {label; kind} *)

  module StringHashtbl = Hashtbl.Make (String)

  let size = 13

  let empty : event_type_value StringHashtbl.t = StringHashtbl.create size

  let reset tbl = StringHashtbl.reset tbl

  let add (label, kind) tbl =
    StringHashtbl.replace tbl label kind ;
    tbl

  let find label tbl = StringHashtbl.find_opt tbl label

  let remove label tbl =
    StringHashtbl.remove tbl label ;
    tbl

  let _show tbl =
    let f key value acc =
      let kind =
        let value_ty, event_type = value in
        let value_ty_str = Plain.unparse_ty value_ty in
        match event_type with
        | InputType -> "Input(" ^ value_ty_str ^ ")"
        | OutputType -> "Output(" ^ value_ty_str ^ ")"
      in
      acc ^ key ^ ": " ^ kind ^ "\n"
    in
    StringHashtbl.fold f tbl ""

  let to_list tbl =
    let f key value acc = (key, value) :: acc in
    let l = StringHashtbl.fold f tbl [] in
    List.filter_map
      (fun (key, value) ->
        match value with
        | Undefined -> None
        | Defined (ty, event_type) -> Some (key, (ty, event_type)) )
      l
end

type template_ty =
  { expr_param_tys: (string * type_expr) list
        (* Types of the expression parameters, e.g: Number, String, etc. *)
  ; event_param_labels: (string * event_label) list
        (* Labels of the event parameters, e.g: A, B, etc. *)
  ; export_tys: (string * event_label) list
        (* Exported events and their types, same as [event_param_labels] *) }

let mk_template_ty ?(expr_param_tys = []) ?(event_param_labels = [])
    ?(export_tys = []) () =
  {expr_param_tys; event_param_labels; export_tys}

let mk_template_ty_from template_def =
  let {params; export; export_types; _} = template_def in
  let expr_param_tys, event_param_labels =
    List.partition_map
      (fun (id, ty, _) ->
        match ty.data with
        | EventTy label -> Right (id.data, label)
        | _ -> Left (id.data, ty) )
      (* (fun (id, param_type) ->
         match param_type with
         | ExprParam (ty, _default) -> Left (id.data, ty)
         | EventParam label -> Right (id.data, label) ) *)
      params
  in
  let export_tys = List.combine (deannotate_list export) export_types in
  mk_template_ty ~expr_param_tys ~event_param_labels ~export_tys ()
