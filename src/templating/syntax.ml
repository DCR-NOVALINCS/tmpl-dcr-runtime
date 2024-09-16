(*
  =============================================================================
  Expressions / Type Expressions
  =============================================================================
*)
type type_expr =
  | UnitTy
  | StringTy
  | IntTy
  | BoolTy
  | EventTy of string
  | RecordTy of (type_expr) record_field list
  | ListTy of type_expr
  (* ADD Template Type *)
  (* | TemplateTy of template_def *)

and expr =
  | Unit
  | True
  | False
  | IntLit of int
  | StringLit of string
  | Parenthesized of expr
  | BinaryOp of expr * expr * binary_op_type
  | UnaryOp of expr * unary_op_type
  | Identifier of string
  | Trigger
  | PropDeref of expr * string
  | List of expr list
  | Record of (expr) record_field list
  (* ADD Template Expr *)
  | Template of template_instance

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

and unary_op_type =
  | Minus
  | Negation

and ('a) record_field = string * 'a

(*
  =============================================================================
  Program
  =============================================================================
*)
and program =
  { template_decls : template_def list
  ; events : event list
  ; template_insts : template_instance list
  ; relations : relation list
  }

and subprogram = event list * template_instance list * relation list

(*
  =============================================================================
  Program Section: template definitions
  =============================================================================
*)

(* TODO: Types for template definition*)
and template_def = {
  export: string list;
  params : (string * type_expr) list;
  graph : subprogram;
  id : string;
}

(*
  =============================================================================
  Program Section: template instantiations
  =============================================================================
*)

(* TODO: Types for template definition*)
and template_instance = {
  args : (string * expr) list;
  x: string list;
  tmpl_id: string;
  tmpl_annotations: template_annotation list;
}

(*
  =============================================================================
  Program Section: template annotations
  =============================================================================
*)

and template_annotation = 
  | When of expr
  | Foreach of string * expr

(*
  =============================================================================
  Program Section: Events
  =============================================================================
*)

and event =
  { info : event_info
  ; io : event_io
  ; marking : event_marking
  ; annotations : template_annotation list
  }

and event_info = string * string (* id : label *)

and event_io =
  | Input of type_expr
  | Output of expr

and event_marking =
  { executed : bool 
  ; pending : bool 
  ; included : bool 
  ; value: expr
  }

(*
  =============================================================================
  Program Section: Relations
  =============================================================================
*)

and relation =
  | ControlRelation of string * expr * string * relation_type * template_annotation list
  | SpawnRelation of string * expr * subprogram * template_annotation list

and relation_type =
  | Condition
  | Include
  | Exclude
  | Milestone
  | Response


(*
  =============================================================================
  Program Section: Type makers
  =============================================================================
*)

let mk_marking ?(executed=false) ?(pending=false) ?(included=true) ?(value=Unit) () = { executed; pending; included; value = value }

let default_marking = mk_marking ()

let mk_event ?(marking=default_marking) ?(annotations = []) ~id ~label io = { info = (id, label); io = io; marking; annotations }

let mk_control_relation ?(annotations = []) ~from ?(guard = True) ~dest t = ControlRelation (from, guard, dest, t, annotations)

let mk_spawn_relation ?(annotations = []) ~from ?(guard= True) subprogram = SpawnRelation (from, guard, subprogram, annotations)

let mk_template_def id params graph ~export = { id; params; graph; export }

let mk_template_inst ?(annotations = []) id args ~x = { tmpl_id = id; args; x; tmpl_annotations=annotations }

let mk_program 
  ?(template_decls=[])
  ?(events=[])
  ?(template_insts=[])
  ?(relations=[])
  _ = 
  { template_decls; events; template_insts; relations }

let mk_subprogram
  ?(events=[])
  ?(template_insts=[])
  ?(relations=[])
  _ = 
  (events, template_insts, relations)

let empty_program = mk_program ()

let empty_subprogram = mk_subprogram ()

let empty_template_inst = mk_template_inst "" [] ~x:[]

(*
  =============================================================================
  Program Section: Pretty Printers
  =============================================================================
*)

let rec string_of_type_expr = function
  | UnitTy -> "unit"
  | StringTy -> "string"
  | IntTy -> "int"
  | BoolTy -> "bool"
  | EventTy s -> s
  | RecordTy fields -> 
    let string_of_field (name, ty) = name ^ " : " ^ (string_of_type_expr ty) in
    "{ " ^ (String.concat "; " (List.map string_of_field fields)) ^ " }"
  | ListTy ty -> "[" ^ (string_of_type_expr ty) ^ "]"

and string_of_expr = function
  | Unit -> "()"
  | True -> "true"
  | False -> "false"
  | IntLit i -> string_of_int i
  | StringLit s -> "\"" ^ s ^ "\""
  | Parenthesized e -> "(" ^ (string_of_expr e) ^ ")"
  | BinaryOp (e1, e2, op) -> 
    let string_of_op = function
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
    (string_of_expr e1) ^ " " ^ (string_of_op op) ^ " " ^ (string_of_expr e2)
  | UnaryOp (e, op) -> 
    let string_of_op = function
      | Minus -> "-"
      | Negation -> "!"
    in
    (string_of_op op) ^ (string_of_expr e)
  | Identifier s -> s
  | Trigger -> "@trigger"
  | PropDeref (e, p) -> (string_of_expr e) ^ "." ^ p
  | List es -> "[" ^ (String.concat ", " (List.map string_of_expr es)) ^ "]"
  | Record fields -> 
    let string_of_field (name, e) = name ^ " = " ^ (string_of_expr e) in
    "{ " ^ (String.concat "; " (List.map string_of_field fields)) ^ " }"
  | _ -> "unknown"

and string_of_event_io = function
  | Input ty -> Printf.sprintf "[?:%s]" (string_of_type_expr ty)
  | Output e -> Printf.sprintf "[%s]" (string_of_expr e)

and string_of_event_marking m = 
  Printf.sprintf "{ ex = %b; res = %b; in = %b; va = %s }" m.executed m.pending m.included (string_of_expr m.value)

and string_of_event ?(abbreviated = true) (e : event) =
  let annots = List.map string_of_template_annotation e.annotations in
  if not abbreviated then
    Printf.sprintf "%s:%s%s %s" (fst e.info) (snd e.info) (string_of_event_io e.io) (string_of_event_marking e.marking)
  else 
    let excluded = if not e.marking.included then "%" else "" in
    let pending = if e.marking.pending then "!" else "" in
    let executed = if e.marking.executed then "✓" else "" in
    let value = string_of_expr e.marking.value in
    Printf.sprintf "%s%s%s%s:%s%s -> %s %s" excluded pending executed (fst e.info) (snd e.info) (string_of_event_io e.io) value (String.concat " | " annots)

and string_of_relation_type = function
  | Condition -> Printf.sprintf "-%s->*"
  | Include -> Printf.sprintf "-%s->+"
  | Exclude -> Printf.sprintf "-%s->%%"
  | Milestone -> Printf.sprintf "-%s-><>"
  | Response -> Printf.sprintf "*-%s->"

and string_of_relation = function
| ControlRelation (from, guard, dest, t, annot) -> 
  let guard = if guard = True then "" else Printf.sprintf "[%s]" (string_of_expr guard) in
  let rel = string_of_relation_type t guard in
  let annot = 
    if List.length annot <= 0 then ""
    else List.map string_of_template_annotation annot
    |> String.concat " | " 
    |> Printf.sprintf "-- %s"
  in
  Printf.sprintf "%s %s %s %s" from rel dest annot
| SpawnRelation (from, guard, subprogram, annot) -> 
  let guard = if guard = True then "" else Printf.sprintf "[%s]" (string_of_expr guard) in
  let rel = Printf.sprintf "-%s->>" guard in
  let annot = 
    if List.length annot <= 0 then ""
    else List.map string_of_template_annotation annot
    |> String.concat " | " 
    |> Printf.sprintf "-- %s"
  in
  Printf.sprintf "%s %s {\n%s\n} %s" from rel (string_of_subprogram ~indent:"  " subprogram) annot

and string_of_template_inst = 
  let string_of_arg (name, e) = Printf.sprintf "%s = %s" name (string_of_expr e) in
  fun { tmpl_id; args; x; tmpl_annotations = annotations } -> 
    let args = List.map string_of_arg args in
    let args = String.concat ", " args in
    let xs = String.concat ", " x in
    let annots = if List.length annotations <= 0 then "" 
    else List.map string_of_template_annotation annotations 
    |> String.concat " | " 
    |> Printf.sprintf "-- %s" in
    Printf.sprintf "%s(%s) => %s %s" tmpl_id args xs annots

and string_of_template_annotation = 
  function 
  | When e -> Printf.sprintf "when %s" (string_of_expr e)
  | Foreach (name, e) -> Printf.sprintf "foreach %s in %s" name (string_of_expr e)

and string_of_subprogram ?(indent = "") (events, _templates, relations) = 
  let string_of_event_list = List.map string_of_event events in
  let string_of_template_inst_list = List.map string_of_template_inst _templates in
  let string_of_relation_list = List.map string_of_relation relations in
  List.flatten [
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
=============================================================================
  Aux functions
=============================================================================
*)

(* the alpha-renaming function *)
let rec _counter = ref 0

and fresh name =
  let res = name ^ "_" ^ (string_of_int !_counter) in
  _counter := !_counter + 1;
  res

and fresh_event event = 
  let (id, label) = event.info in
  let new_id = fresh id in
  { event with info = (new_id, label) }

and fresh_event_ids events relations _exports_mapping  = 
  let fresh_events = List.map fresh_event events in
  let _fresh_relations = List.map (function
    | ControlRelation (from, guard, dest, t, _annot) -> 
      let new_from = fresh from in
      let new_dest = fresh dest in
      ControlRelation (new_from, guard, new_dest, t, _annot)
    | SpawnRelation (from, guard, subprogram, _annot) -> 
      let new_from = fresh from in
      SpawnRelation (new_from, guard, subprogram, _annot)
  ) relations in
  Ok (fresh_events, relations)
  
and record_event event = 
  let { marking; _ } = event in
  Record [
    ("value", marking.value)
  ]