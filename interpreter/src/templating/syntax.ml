open Ppx_yojson_conv_lib.Yojson_conv

(*
   =============================================================================
     Annotation related types & modules
   =============================================================================
*)

module Lexing = struct
  include Lexing

  let position_to_yojson pos =
    `Assoc
      [
        ("pos_cnum", `Int pos.pos_cnum);
        ("pos_bol", `Int pos.pos_bol);
        ("pos_lnum", `Int pos.pos_lnum);
      ]

  let position_of_yojson = function
    | `Assoc
        [
          ("pos_cnum", `Int pos_cnum);
          ("pos_bol", `Int pos_bol);
          ("pos_lnum", `Int pos_lnum);
        ] ->
        { pos_cnum; pos_bol; pos_lnum; pos_fname = "" }
    | _ -> failwith "position_of_yojson: invalid input"

  let yojson_of_position pos = position_to_yojson pos
end

type loc =
  | Nowhere
  | Location of Lexing.position * Lexing.position * string option (* (start_pos, end_pos, filename) *)
[@@deriving yojson]

type 'a annotated = { data : 'a; loc : loc; ty : type_expr' option ref }
[@@deriving yojson]

(*
  =============================================================================
  Expressions / Type Expressions
  =============================================================================
*)
and type_expr = type_expr' annotated [@@deriving yojson]

and type_expr' =
  | UnitTy
  | StringTy
  | IntTy
  | BoolTy
  | EventTy of string
  | RecordTy of type_expr record_field list
  | ListTy of type_expr
(* ADD Template Type *)
(* | TemplateTy of template_def *)
[@@deriving yojson]

and expr = expr' annotated [@@deriving yojson]

and expr' =
  | Unit
  | True
  | False
  | IntLit of int
  | StringLit of string
  | Parenthesized of expr
  | BinaryOp of expr * expr * binary_op_type
  | UnaryOp of expr * unary_op_type
  | Identifier of string annotated
  | Trigger
  | PropDeref of expr * string annotated
  | List of expr list
  | Record of expr record_field list
  (* ADD Template Expr *)
  | Template of template_instance
[@@deriving yojson]

and binary_op_type =
  | Add
  | Mult
  | Eq
  | NotEq
  | GreaterThan
  | GreaterOrEqual
  | LessThan
  | LessOrEqual
  | And
  | Or
[@@deriving yojson]

and unary_op_type = Minus | Negation [@@deriving yojson]
and 'a record_field = string annotated * 'a [@@deriving yojson]

(*
  =============================================================================
  Program
  =============================================================================
*)
and program = {
  template_decls : template_def list;
  events : event list;
  template_insts : template_instance list;
  relations : relation list;
}
[@@deriving yojson]

and subprogram = event list * template_instance list * relation list
[@@deriving yojson]

(*
  =============================================================================
  Program Section: template definitions
  =============================================================================
*)
and template_def = {
  export : event_id list;
  params : (string annotated * type_expr * expr option) list;
  export_types : type_expr list;
  graph : subprogram;
  id : string annotated;
}
[@@deriving yojson]

(*
  =============================================================================
  Program Section: template instantiations
  =============================================================================
*)
and template_instance = {
  args : (string annotated * expr) list;
  x : event_id list;
  tmpl_id : string annotated;
  tmpl_annotations : template_annotation' list;
}
[@@deriving yojson]

(*
  =============================================================================
  Program Section: template annotations
  =============================================================================
*)
and template_annotation = template_annotation' annotated [@@deriving yojson]

and template_annotation' = When of expr | Foreach of string annotated * expr
[@@deriving yojson]

(*
  =============================================================================
  Program Section: Events
  =============================================================================
*)
and event = event' annotated [@@deriving yojson]

and event' = {
  info : event_info';
  io : event_io;
  marking : event_marking;
  annotations : template_annotation' list;
}
[@@deriving yojson]

and event_id = string annotated [@@deriving yojson]
and event_label = string annotated [@@deriving yojson]
and event_info' = event_id * event_label [@@deriving yojson]
and event_io = event_io' annotated [@@deriving yojson]
and event_io' = Input of type_expr | Output of expr [@@deriving yojson]
and event_marking = event_marking' annotated [@@deriving yojson]

and event_marking' = {
  executed : bool annotated;
  pending : bool annotated;
  included : bool annotated;
  value : expr;
}
[@@deriving yojson]

(*
  =============================================================================
  Program Section: Relations
  =============================================================================
*)
and relation = relation' annotated [@@deriving yojson]

and relation' =
  | ControlRelation of
      event_id * expr * event_id * relation_type * template_annotation' list
  | SpawnRelation of event_id * expr * subprogram * template_annotation' list
[@@deriving yojson]

and relation_type = Condition | Include | Exclude | Milestone | Response
[@@deriving yojson]

(*
  =============================================================================
  Program Section: Type makers
  =============================================================================
*)

let annotate ?(loc = Nowhere) ?(ty = None) data = { data; loc; ty = ref ty }
let deannotate { data; _ } = data
let deannotate_list lst = List.map deannotate lst

let mk_marking ?(executed = false) ?(pending = false) ?(included = true)
    ?(value = Unit) () =
  {
    executed = annotate executed;
    pending = annotate pending;
    included = annotate included;
    value = annotate value;
  }

let default_marking = mk_marking ()
let default_marking_excl = mk_marking ~included:false ()
let default_marking_pend = mk_marking ~pending:true ()
let default_marking_pend_excl = mk_marking ~pending:true ~included:false ()

let mk_event ?(marking = default_marking) ?(annotations = []) info io =
  annotate { info; io; marking = annotate marking; annotations }

let mk_ctrl_relation ?(annotations = []) ~from ?(guard = annotate True) ~dest t
    =
  annotate @@ ControlRelation (from, guard, dest, t, annotations)

let mk_ctrl_relations ?(annotations = []) left_ids expr right_ids t =
  List.concat_map
    (fun id1 ->
      List.map
        (fun id2 ->
          mk_ctrl_relation ~from:id1 ~guard:expr ~dest:id2 ~annotations t)
        right_ids)
    left_ids

let mk_spawn_relation ?(annotations = []) ~from ?(guard = annotate True)
    subprogram =
  annotate @@ SpawnRelation (from, guard, subprogram, annotations)

let mk_spawn_relations ?(annotations = []) left_ids expr prog =
  List.map
    (fun id -> mk_spawn_relation ~from:id ~guard:expr ~annotations prog)
    left_ids

let mk_template_def id params types graph ~export =
  {
    id = annotate id;
    params =
      List.map
        (fun (name, ty, def_expr) ->
          (annotate name, annotate ty, Some def_expr))
        params;
    export_types = List.map annotate types;
    graph;
    export = List.map annotate export;
  }

let mk_template_inst ?(annotations = []) id args ~x =
  {
    tmpl_id = annotate id;
    args = List.map (fun (name, e) -> (annotate name, annotate e)) args;
    x = List.map annotate x;
    tmpl_annotations = annotations;
  }

let mk_program ?(template_decls = []) ?(events = []) ?(template_insts = [])
    ?(relations = []) _ =
  { template_decls; events; template_insts; relations }

let mk_subprogram ?(events = []) ?(template_insts = []) ?(relations = []) _ =
  (events, template_insts, relations)

let empty_program = mk_program ()
let empty_subprogram = mk_subprogram ()
let empty_template_inst = mk_template_inst "" [] ~x:[]

(*
  =============================================================================
  Program Section: Pretty Printers
  =============================================================================
*)

let string_of_pos pos =
  let line = pos.Lexing.pos_lnum in
  let start_char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Printf.sprintf "%d:%d" line start_char

let string_of_loc loc =
  match loc with
  | Nowhere -> "?"
  | Location (start_pos, end_pos, filename) ->
      let filename = Option.value filename ~default:"" in
      let start = string_of_pos start_pos in
      let end_ = string_of_pos end_pos in
      Printf.sprintf "%s:%s:%s" filename start end_

let rec string_of_type_expr ty =
  match ty.data with
  | UnitTy -> "()"
  | StringTy -> "String"
  | IntTy -> "Number"
  | BoolTy -> "Boolean"
  | EventTy s -> s
  | RecordTy fields ->
      let string_of_field (name, ty) =
        name.data ^ " : " ^ string_of_type_expr ty
      in
      "{ " ^ String.concat "; " (List.map string_of_field fields) ^ " }"
  | ListTy ty -> "[" ^ string_of_type_expr ty ^ "]"

and string_of_expr expr =
  match expr.data with
  | Unit -> "()"
  | True -> "true"
  | False -> "false"
  | IntLit i -> string_of_int i
  | StringLit s -> "\"" ^ s ^ "\""
  | Parenthesized e -> "(" ^ string_of_expr e ^ ")"
  | BinaryOp (e1, e2, op) ->
      let op_str =
        match op with
        | Add -> "+"
        | Mult -> "*"
        | Eq -> "=="
        | NotEq -> "!="
        | GreaterThan -> ">"
        | GreaterOrEqual -> ">="
        | LessThan -> "<"
        | LessOrEqual -> "<="
        | And -> "&&"
        | Or -> "||"
      in
      Printf.sprintf "%s %s %s" (string_of_expr e1) op_str (string_of_expr e2)
  | UnaryOp (e, op) ->
      let op_str = match op with Minus -> "-" | Negation -> "!" in
      Printf.sprintf "%s%s" op_str (string_of_expr e)
  | Identifier s -> s.data
  | Trigger -> "trigger"
  | PropDeref (e, prop) -> Printf.sprintf "%s.%s" (string_of_expr e) prop.data
  | List es -> "[" ^ String.concat ", " (List.map string_of_expr es) ^ "]"
  | Record fields ->
      let string_of_field (name, e) = name.data ^ " = " ^ string_of_expr e in
      "{ " ^ String.concat "; " (List.map string_of_field fields) ^ " }"
  | Template t -> string_of_template_inst t

and string_of_event_io io =
  match io.data with
  | Input ty -> Printf.sprintf "[?: %s]" (string_of_type_expr ty)
  | Output e -> Printf.sprintf "[%s]" (string_of_expr e)

and string_of_event_marking m =
  let m = m.data in
  Printf.sprintf "{ ex = %b; res = %b; in = %b; va = %s }" m.executed.data
    m.pending.data m.included.data (string_of_expr m.value)

and string_of_event ?(abbreviated = true) e =
  (* let open Misc.Printing in *)
  (* Logger.debug @@ Printf.sprintf "event: %s" (fst e.data.info).data; *)
  let annots = List.map string_of_template_annotation e.data.annotations in
  let id, label = e.data.info in
  if not abbreviated then
    Printf.sprintf "%s:%s%s %s" id.data label.data
      (string_of_event_io e.data.io)
      (string_of_event_marking e.data.marking)
  else
    let marking = e.data.marking.data in
    let excluded = if not marking.included.data then "%" else "" in
    let pending = if marking.pending.data then "!" else "" in
    let executed = if marking.executed.data then "✓" else "" in
    let value = string_of_expr marking.value in
    (* Logger.debug @@ Printf.sprintf "tokens: |%s| |%s| |%s|" excluded pending executed; *)
    Printf.sprintf "%s%s%s%s:%s%s -> %s %s" excluded pending executed id.data
      label.data
      (string_of_event_io e.data.io)
      value
      (String.concat " | " annots)

and string_of_relation_type = function
  | Condition -> Printf.sprintf "-%s->*"
  | Include -> Printf.sprintf "-%s->+"
  | Exclude -> Printf.sprintf "-%s->%%"
  | Milestone -> Printf.sprintf "-%s-><>"
  | Response -> Printf.sprintf "*-%s->"

and string_of_relation relation =
  match relation.data with
  | ControlRelation (from, guard, dest, t, annot) ->
      let guard =
        if guard.data = True then ""
        else Printf.sprintf "[%s]" (string_of_expr guard)
      in
      let rel = string_of_relation_type t guard in
      let annot =
        if List.length annot <= 0 then ""
        else
          List.map string_of_template_annotation annot
          |> String.concat " | " |> Printf.sprintf "-- %s"
      in
      Printf.sprintf "%s %s %s %s" from.data rel dest.data annot
  | SpawnRelation (from, guard, subprogram, annot) ->
      let guard =
        if guard.data = True then ""
        else Printf.sprintf "[%s]" (string_of_expr guard)
      in
      let rel = Printf.sprintf "-%s->>" guard in
      let annot =
        if List.length annot <= 0 then ""
        else
          List.map string_of_template_annotation annot
          |> String.concat " | " |> Printf.sprintf "-- %s"
      in
      Printf.sprintf "%s %s {\n%s\n} %s" from.data rel
        (string_of_subprogram ~indent:"  " subprogram)
        annot

and string_of_template_inst =
  let string_of_arg (name, e) =
    Printf.sprintf "%s = %s" name.data (string_of_expr e)
  in
  fun { tmpl_id; args; x; tmpl_annotations = annotations } ->
    let args = List.map string_of_arg args in
    let args = String.concat ", " args in
    let xs = List.map (fun x -> x.data) x in
    let xs =
      if List.length xs <= 0 then ""
      else Printf.sprintf " => %s" (String.concat ", " xs)
    in
    let annots =
      if List.length annotations <= 0 then ""
      else
        List.map string_of_template_annotation annotations
        |> String.concat " | " |> Printf.sprintf "-- %s"
    in
    Printf.sprintf "%s(%s) %s %s" tmpl_id.data args xs annots

and string_of_template_annotation = function
  | When e -> Printf.sprintf "when %s" (string_of_expr e)
  | Foreach (name, e) ->
      Printf.sprintf "foreach %s in %s" name.data (string_of_expr e)

and string_of_subprogram ?(indent = "") (events, templates, relations) =
  let string_of_event_list = List.map string_of_event events in
  let string_of_template_inst_list =
    List.map string_of_template_inst templates
  in
  let string_of_relation_list = List.map string_of_relation relations in
  List.flatten
    [
      string_of_event_list;
      string_of_template_inst_list;
      string_of_relation_list;
    ]
  |> List.map (Printf.sprintf "%s%s" indent)
  |> String.concat "\n"

and string_of_program p =
  let { template_decls = _; events; template_insts; relations } = p in
  string_of_subprogram (events, template_insts, relations)

(*
===============================================================
  Alpha-renaming functions
===============================================================
*)

open Misc.Monads
open Misc.Printing

let rec _counter = ref 0

and fresh name =
  let res = name ^ "_" ^ string_of_int !_counter in
  _counter := !_counter + 1;
  res

and fresh_event event =
  let id, label = event.data.info in
  let new_id = fresh id.data in
  {
    event with
    data =
      {
        event.data with
        info = (annotate ~loc:id.loc ~ty:!(id.ty) new_id, label);
      };
  }

and change_info_event ~new_id ~new_label event =
  let id, label = event.data.info in
  {
    event with
    data =
      {
        event.data with
        info = ({ id with data = new_id }, { label with data = new_label });
      };
  }

and get_relation_of_event id relation =
  match relation.data with
  | ControlRelation (from, _, dest, _, _) ->
      from.data = id.data || dest.data = id.data
  | SpawnRelation (from, _, _, _) -> from.data = id.data

and change_relation old_id new_id relation =
  match relation.data with
  | ControlRelation (from, guard, dest, t, annot) ->
      let new_from = if from.data = old_id.data then new_id else from in
      let new_dest = if dest.data = old_id.data then new_id else dest in
      {
        relation with
        data = ControlRelation (new_from, guard, new_dest, t, annot);
      }
  | SpawnRelation (from, guard, subprogram, annot) ->
      let new_from = if from.data = old_id.data then new_id else from in
      {
        relation with
        data = SpawnRelation (new_from, guard, subprogram, annot);
      }

and fresh_event_ids events relations exports_mapping =
  map_result
    (fun event ->
      let id, label = event.data.info in
      let export_id =
        match List.assoc_opt id.data exports_mapping with
        | None -> id
        | Some new_id ->
            Logger.group "export mapping";
            Logger.debug
            @@ Printf.sprintf "changing from %s to %s" id.data new_id.data;
            Logger.end_group ();
            new_id
      in
      let fresh_id = annotate ~loc:id.loc ~ty:!(id.ty) (fresh export_id.data) in
      let fresh_event =
        change_info_event ~new_id:fresh_id.data ~new_label:label.data event
      in
      Ok (id, fresh_id, fresh_event))
    events
  >>= fun events_mapping ->
  map_result
    (fun relation ->
      fold_left_result
        (fun relation (old_id, new_id, _) ->
          if get_relation_of_event old_id relation then (
            Logger.group
            @@ Printf.sprintf "Freshening relation %s"
                 (string_of_relation relation);
            Logger.debug
            @@ Printf.sprintf "Changing old %s to new %s" old_id.data
                 new_id.data;
            Logger.end_group ();
            Ok (change_relation old_id new_id relation))
          else Ok relation)
        relation events_mapping)
    relations
  >>= fun fresh_relations ->
  let fresh_events = List.map (fun (_, _, e) -> e) events_mapping in
  Ok (fresh_events, fresh_relations)

(*
=============================================================================
  Aux functions
=============================================================================
*)

and record_event event =
  let { marking; _ } = event.data in
  annotate ~loc:event.loc ~ty:!(event.ty)
    (Record [ (annotate ~loc:event.loc "value", marking.data.value) ])
