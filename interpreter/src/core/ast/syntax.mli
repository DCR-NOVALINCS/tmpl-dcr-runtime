module Lexing : sig
  type position = Lexing.position =
    {pos_fname: string; pos_lnum: int; pos_bol: int; pos_cnum: int}

  val dummy_pos : position

  type lexbuf = Lexing.lexbuf =
    { refill_buff: lexbuf -> unit
    ; mutable lex_buffer: bytes
    ; mutable lex_buffer_len: int
    ; mutable lex_abs_pos: int
    ; mutable lex_start_pos: int
    ; mutable lex_curr_pos: int
    ; mutable lex_last_pos: int
    ; mutable lex_last_action: int
    ; mutable lex_eof_reached: bool
    ; mutable lex_mem: int array
    ; mutable lex_start_p: position
    ; mutable lex_curr_p: position }

  val from_channel : ?with_positions:bool -> in_channel -> lexbuf

  val from_string : ?with_positions:bool -> string -> lexbuf

  val from_function : ?with_positions:bool -> (bytes -> int -> int) -> lexbuf

  val set_position : lexbuf -> position -> unit

  val set_filename : lexbuf -> string -> unit

  val with_positions : lexbuf -> bool

  val lexeme : lexbuf -> string

  val lexeme_char : lexbuf -> int -> char

  val lexeme_start : lexbuf -> int

  val lexeme_end : lexbuf -> int

  val lexeme_start_p : lexbuf -> position

  val lexeme_end_p : lexbuf -> position

  val new_line : lexbuf -> unit

  val flush_input : lexbuf -> unit

  val sub_lexeme : lexbuf -> int -> int -> string

  val sub_lexeme_opt : lexbuf -> int -> int -> string option

  val sub_lexeme_char : lexbuf -> int -> char

  val sub_lexeme_char_opt : lexbuf -> int -> char option

  type lex_tables = Lexing.lex_tables =
    { lex_base: string
    ; lex_backtrk: string
    ; lex_default: string
    ; lex_trans: string
    ; lex_check: string
    ; lex_base_code: string
    ; lex_backtrk_code: string
    ; lex_default_code: string
    ; lex_trans_code: string
    ; lex_check_code: string
    ; lex_code: string }

  val engine : lex_tables -> int -> lexbuf -> int

  val new_engine : lex_tables -> int -> lexbuf -> int

  val position_to_yojson :
    position -> [> `Assoc of (string * [> `Int of int | `String of string]) list]

  val position_of_yojson :
       [> `Assoc of (string * [> `Int of int | `String of string]) list]
    -> position

  val yojson_of_position :
    position -> [> `Assoc of (string * [> `Int of int | `String of string]) list]
end

type loc =
  | Nowhere
  | Location of Lexing.position * Lexing.position * string option

val loc_of_yojson : Yojson.Safe.t -> loc

val yojson_of_loc : loc -> Yojson.Safe.t

val append_loc : loc -> loc -> loc

val append_locs : loc list -> loc

type 'a annotated = {data: 'a; loc: loc; ty: type_expr' option ref}

and program =
  { template_decls: template_def list
  ; events: event list
  ; template_insts: template_instance list
  ; relations: relation list
  ; annotations: template_annotation' list }

and subprogram =
  event list
  * template_instance list
  * relation list
  * template_annotation' list

and type_expr = type_expr' annotated

and type_expr' =
  | UnitTy
  | StringTy
  | IntTy
  | BoolTy
  | EventTy of string annotated
  | RecordTy of type_expr record_field list
  | ListTy of type_expr'

and expr = expr' annotated

and expr' =
  | Unit
  | BoolLit of bool
  | IntLit of int
  | StringLit of string
  | Parenthesized of expr
  | BinaryOp of expr * expr * binary_op_type
  | UnaryOp of expr * unary_op_type
  | Identifier of string annotated
  | Trigger
  | PropDeref of expr * string annotated
  | List of expr list
  | Range of expr * expr
  | Record of expr record_field list
  | EventRef of event ref
  | Ref of expr ref

and binary_op_type =
  | Add
  | Sub
  | Mult
  | Div
  | Eq
  | NotEq
  | GreaterThan
  | GreaterOrEqual
  | LessThan
  | LessOrEqual
  | And
  | Or

and unary_op_type = Minus | Negation

and 'a record_field = string annotated * 'a

and template_param' = string annotated * type_expr * expr option

and template_def =
  { export: event_id list
  ; params: template_param' list
  ; export_types: event_label list
  ; graph: subprogram
  ; id: string annotated }

and template_arg = string annotated * expr

and template_instance = template_instance' annotated

and template_instance' =
  {args: template_arg list; x: event_id list; tmpl_id: string annotated}

and template_annotation = template_annotation' annotated

and template_annotation' =
  | IfElse of
      {condition: expr; then_branch: subprogram; else_branch: subprogram option}
  | Foreach of string annotated * expr * subprogram

and event = event' annotated

and event' = {info: event_info'; io: event_io; marking: event_marking}

and event_id = string annotated

and event_label = string annotated

and event_info' = event_id * event_label

and event_io = event_io' annotated

and event_io' = Input of type_expr | Output of expr

and event_type' = InputType | OutputType

and event_marking = event_marking' annotated

and event_marking' =
  { executed: bool annotated
  ; pending: bool annotated
  ; included: bool annotated
  ; value: expr ref }

and relation = relation' annotated

and relation' =
  | ControlRelation of event_id * expr * event_id * relation_type
  | SpawnRelation of event_id * expr * subprogram

and relation_type =
  | Condition
  | Include
  | Exclude
  | Milestone
  | Response
  | Cancel

val annotated_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a annotated

val program_of_yojson : Yojson.Safe.t -> program

val subprogram_of_yojson : Yojson.Safe.t -> subprogram

val type_expr_of_yojson : Yojson.Safe.t -> type_expr

val type_expr'_of_yojson : Yojson.Safe.t -> type_expr'

val expr_of_yojson : Yojson.Safe.t -> expr

val expr'_of_yojson : Yojson.Safe.t -> expr'

val binary_op_type_of_yojson : Yojson.Safe.t -> binary_op_type

val unary_op_type_of_yojson : Yojson.Safe.t -> unary_op_type

val record_field_of_yojson :
  (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a record_field

val template_param'_of_yojson : Yojson.Safe.t -> template_param'

val template_def_of_yojson : Yojson.Safe.t -> template_def

val template_arg_of_yojson : Yojson.Safe.t -> template_arg

val template_instance_of_yojson : Yojson.Safe.t -> template_instance

val template_instance'_of_yojson : Yojson.Safe.t -> template_instance'

val template_annotation_of_yojson : Yojson.Safe.t -> template_annotation

val template_annotation'_of_yojson : Yojson.Safe.t -> template_annotation'

val event_of_yojson : Yojson.Safe.t -> event

val event'_of_yojson : Yojson.Safe.t -> event'

val event_id_of_yojson : Yojson.Safe.t -> event_id

val event_label_of_yojson : Yojson.Safe.t -> event_label

val event_info'_of_yojson : Yojson.Safe.t -> event_info'

val event_io_of_yojson : Yojson.Safe.t -> event_io

val event_io'_of_yojson : Yojson.Safe.t -> event_io'

val event_type'_of_yojson : Yojson.Safe.t -> event_type'

val event_marking_of_yojson : Yojson.Safe.t -> event_marking

val event_marking'_of_yojson : Yojson.Safe.t -> event_marking'

val relation_of_yojson : Yojson.Safe.t -> relation

val relation'_of_yojson : Yojson.Safe.t -> relation'

val relation_type_of_yojson : Yojson.Safe.t -> relation_type

val yojson_of_annotated : ('a -> Yojson.Safe.t) -> 'a annotated -> Yojson.Safe.t

val yojson_of_program : program -> Yojson.Safe.t

val yojson_of_subprogram : subprogram -> Yojson.Safe.t

val yojson_of_type_expr : type_expr -> Yojson.Safe.t

val yojson_of_type_expr' : type_expr' -> Yojson.Safe.t

val yojson_of_expr : expr -> Yojson.Safe.t

val yojson_of_expr' : expr' -> Yojson.Safe.t

val yojson_of_binary_op_type : binary_op_type -> Yojson.Safe.t

val yojson_of_unary_op_type : unary_op_type -> Yojson.Safe.t

val yojson_of_record_field :
  ('a -> Yojson.Safe.t) -> 'a record_field -> Yojson.Safe.t

val yojson_of_template_param' : template_param' -> Yojson.Safe.t

val yojson_of_template_def : template_def -> Yojson.Safe.t

val yojson_of_template_arg : template_arg -> Yojson.Safe.t

val yojson_of_template_instance : template_instance -> Yojson.Safe.t

val yojson_of_template_instance' : template_instance' -> Yojson.Safe.t

val yojson_of_template_annotation : template_annotation -> Yojson.Safe.t

val yojson_of_template_annotation' : template_annotation' -> Yojson.Safe.t

val yojson_of_event : event -> Yojson.Safe.t

val yojson_of_event' : event' -> Yojson.Safe.t

val yojson_of_event_id : event_id -> Yojson.Safe.t

val yojson_of_event_label : event_label -> Yojson.Safe.t

val yojson_of_event_info' : event_info' -> Yojson.Safe.t

val yojson_of_event_io : event_io -> Yojson.Safe.t

val yojson_of_event_io' : event_io' -> Yojson.Safe.t

val yojson_of_event_type' : event_type' -> Yojson.Safe.t

val yojson_of_event_marking : event_marking -> Yojson.Safe.t

val yojson_of_event_marking' : event_marking' -> Yojson.Safe.t

val yojson_of_relation : relation -> Yojson.Safe.t

val yojson_of_relation' : relation' -> Yojson.Safe.t

val yojson_of_relation_type : relation_type -> Yojson.Safe.t

val annotate : ?loc:loc -> ?ty:type_expr' option -> 'a -> 'a annotated

val deannotate : 'a annotated -> 'a

val deannotate_list : 'a annotated list -> 'a list

val mk_loc : ?filename:string -> Lexing.position -> Lexing.position -> loc

val mk_pos : ?filename:string -> int -> int -> Lexing.position

val mk_marking :
     ?executed:bool
  -> ?pending:bool
  -> ?included:bool
  -> ?value:expr'
  -> unit
  -> event_marking'

val default_marking : event_marking'

val default_marking_excl : event_marking'

val default_marking_pend : event_marking'

val default_marking_pend_excl : event_marking'

val mk_event :
  ?marking:event_marking' -> event_info' -> event_io -> event' annotated

val mk_ctrl_relation :
     from:event_id
  -> ?guard:expr
  -> dest:event_id
  -> relation_type
  -> relation' annotated

val mk_ctrl_relations :
     event_id list
  -> expr
  -> event_id list
  -> relation_type
  -> relation' annotated list

val mk_spawn_relation :
  from:event_id -> ?guard:expr -> subprogram -> relation' annotated

val mk_spawn_relations :
  event_id list -> expr -> subprogram -> relation' annotated list

val mk_program :
     ?template_decls:template_def list
  -> ?events:event list
  -> ?template_insts:template_instance list
  -> ?relations:relation list
  -> ?annotations:template_annotation' list
  -> 'a
  -> program

val to_program :
     event list
     * template_instance list
     * relation list
     * template_annotation' list
  -> program

val mk_subprogram :
     ?events:'a list
  -> ?template_insts:'b list
  -> ?relations:'c list
  -> ?annotations:'d list
  -> 'e
  -> 'a list * 'b list * 'c list * 'd list

val to_subprogram :
     program
  -> event list
     * template_instance list
     * relation list
     * template_annotation' list

val empty_program : program

val empty_subprogram : 'a list * 'b list * 'c list * 'd list

val mk_record : (string * expr') list -> expr' annotated

val get_event_type : event_io' annotated -> event_type'

val show_event_type' : string -> event_type' -> string

val trigger_id : string
