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

and expr =
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
  id : string;
  export: string list;
  params : (string * type_expr) list;
  graph : subprogram
}

(*
  =============================================================================
  Program Section: template instantiations
  =============================================================================
*)

(* TODO: Types for template definition*)
and template_instance = {
  args : (string * expr) list;
  x: string list
}

(*
  =============================================================================
  Program Section: Events
  =============================================================================
*)

and event =
  { info : event_info
  ; io : event_io
  ; marking : (unit) event_marking
  }

and event_info = string * string (* id : label *)

and event_io =
  | Input of type_expr
  | Output of expr

and ('a) event_marking =
  { executed : bool 
  ; pending : bool 
  ; included : bool 
  ; value: 'a
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
