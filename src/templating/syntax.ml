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
  (* | ListTyEmpty *)
  (* ADD Template Type *)

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

and binary_op_type =
  | Add
  | Mult
  | Eq
  | NotEq
  | GreaterThan
  | LessThan
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
}

(*
  =============================================================================
  Program Section: Events
  =============================================================================
*)

and event =
  { info : event_info
  ; io : event_io
  ; marking : event_marking
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
  | ControlRelation of string * expr * string * relation_type
  | SpawnRelation of string * expr * subprogram

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

let mk_event ?(marking=default_marking) ~id ~label io = { info = (id, label); io = io; marking }

let mk_control_relation ~from ?(guard = True) ~dest t = ControlRelation (from, guard, dest, t)

let mk_spawn_relation ~from ?(guard= True) subprogram = SpawnRelation (from, guard, subprogram)

let mk_template_def id params graph ~export = { id; params; graph; export }

let mk_template_inst id args ~x = { tmpl_id = id; args; x }

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
      | LessThan -> "<"
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
  | Trigger -> "trigger"
  | PropDeref (e, p) -> (string_of_expr e) ^ "." ^ p
  | List es -> "[" ^ (String.concat ", " (List.map string_of_expr es)) ^ "]"
  | Record fields -> 
    let string_of_field (name, e) = name ^ " = " ^ (string_of_expr e) in
    "{ " ^ (String.concat "; " (List.map string_of_field fields)) ^ " }"

and string_of_event_io = function
  | Input ty -> Printf.sprintf "[?:%s]" (string_of_type_expr ty)
  | Output e -> Printf.sprintf "[%s]" (string_of_expr e)

and string_of_event_marking m = 
  Printf.sprintf "{ ex = %b; res = %b; in = %b; va = %s }" m.executed m.pending m.included (string_of_expr m.value)

and string_of_event ?(abbreviated = true) (e : event) =
  if not abbreviated then
    Printf.sprintf "%s:%s%s %s" (fst e.info) (snd e.info) (string_of_event_io e.io) (string_of_event_marking e.marking)
  else 
    let excluded = if not e.marking.included then "%" else "" in
    let pending = if e.marking.pending then "!" else "" in
    let executed = if e.marking.executed then "✓" else "" in
    let value = string_of_expr e.marking.value in
    Printf.sprintf "%s%s%s%s:%s%s -> %s" excluded pending executed (fst e.info) (snd e.info) (string_of_event_io e.io) value

and string_of_relation_type = function
  | Condition -> Printf.sprintf "-%s->*"
  | Include -> Printf.sprintf "-%s->+"
  | Exclude -> Printf.sprintf "-%s->%%"
  | Milestone -> Printf.sprintf "-%s-><>"
  | Response -> Printf.sprintf "*-%s->"

and string_of_relation = function
| ControlRelation (from, guard, dest, t) -> 
  let guard = Printf.sprintf "[%s]" (string_of_expr guard) in
  let rel = string_of_relation_type t guard in
  Printf.sprintf "%s %s %s" from rel dest
| SpawnRelation (from, guard, subprogram) -> 
  let guard = Printf.sprintf "[%s]" (string_of_expr guard) in
  let rel = Printf.sprintf "-%s->>" guard in
  Printf.sprintf "%s %s {\n%s\n}" from rel (string_of_subprogram subprogram)

and string_of_subprogram (events, _templates, relations) = 
  Printf.sprintf "%s\n;\n%s" 
    (String.concat "\n" (List.map string_of_event events))
    (* (String.concat "; " []) *)
    (String.concat "\n" (List.map string_of_relation relations))

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

and _fresh name =
  let res = name ^ "_" ^ (string_of_int !_counter) in
  _counter := !_counter + 1;
  res

and fresh_event event = 
  let (id, label) = event.info in
  let new_id = _fresh id in
  { event with info = (new_id, label) }

and fresh_event_ids events relations = 
  Ok (events |> List.map fresh_event, relations)
  
and record_event event = 
  let { marking; _ } = event in
  Record [
    ("value", marking.value)
  ]