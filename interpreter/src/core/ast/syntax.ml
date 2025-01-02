open Ppx_yojson_conv_lib.Yojson_conv

(* =============================================================================
   Annotation related types & modules
   ============================================================================= *)

module Lexing = struct
  include Lexing

  let position_to_yojson pos =
    `Assoc
      [ ("pos_cnum", `Int pos.pos_cnum)
      ; ("pos_bol", `Int pos.pos_bol)
      ; ("pos_lnum", `Int pos.pos_lnum)
      ; ("pos_fname", `String pos.pos_fname) ]

  let position_of_yojson = function
    | `Assoc
        [ ("pos_cnum", `Int pos_cnum)
        ; ("pos_bol", `Int pos_bol)
        ; ("pos_lnum", `Int pos_lnum)
        ; ("pos_fname", `String pos_fname) ] ->
        {pos_cnum; pos_bol; pos_lnum; pos_fname}
    | _ -> failwith "position_of_yojson: invalid input"

  let yojson_of_position pos = position_to_yojson pos
end

type loc =
  | Nowhere
  | Location of
      Lexing.position
      * Lexing.position
      * string option (* (start_pos, end_pos, filename) *)
[@@deriving yojson]

let append_loc loc1 loc2 =
  match (loc1, loc2) with
  | Nowhere, Nowhere -> Nowhere
  | Nowhere, l | l, Nowhere -> l
  | Location (start1, _end1, filename1), Location (_start2, end2, filename2) ->
      Location
        ( start1
        , end2
        , match (filename1, filename2) with
          | Some f1, Some f2 when f1 = f2 -> Some f1
          | _ -> None )

let append_locs l = List.fold_left append_loc Nowhere l

type 'a annotated = {data: 'a; loc: loc; ty: type_expr' option ref}
[@@deriving yojson]

(* =============================================================================
   Program
   ============================================================================= *)
and program =
  { template_decls: template_def list
  ; events: event list
  ; template_insts: template_instance list
  ; relations: relation list
  ; annotations: template_annotation' list }
[@@deriving yojson]

and subprogram =
  event list
  * template_instance list
  * relation list
  * template_annotation' list
[@@deriving yojson]

(* =============================================================================
   Expressions / Type Expressions
   ============================================================================= *)
and type_expr = type_expr' annotated [@@deriving yojson]

and type_expr' =
  | UnitTy
  | StringTy
  | IntTy
  | BoolTy
  | EventTy of string
  | RecordTy of type_expr record_field list
  | ListTy of type_expr'
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
  | Range of expr * expr
  | Record of expr record_field list
  | EventRef of event ref
  | Ref of expr ref
[@@deriving yojson]

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
[@@deriving yojson]

and unary_op_type = Minus | Negation [@@deriving yojson]

and 'a record_field = string annotated * 'a [@@deriving yojson]

(* =============================================================================
   Program Section: template definitions
   ============================================================================= *)
and template_param_type = template_param_type' annotated [@@deriving yojson]

and template_param_type' =
  | ExprParam of type_expr * expr option
  | EventParam of event_label
[@@deriving yojson]

and template_param' = string annotated * type_expr * expr option
[@@deriving yojson]

and template_def =
  { export: event_id list
  ; params: template_param' list
  ; export_types: event_label list
  ; graph: subprogram
  ; id: string annotated }
[@@deriving yojson]

(* =============================================================================
   Program Section: template instantiations
   ============================================================================= *)
and template_arg_type = ExprArg of expr | EventArg of event_id
[@@deriving yojson]

and template_arg = string annotated * expr [@@deriving yojson]

and template_instance = template_instance' annotated [@@deriving yojson]

and template_instance' =
  { args: template_arg list
  ; x: event_id list
  ; tmpl_id: string annotated
        (* ; tmpl_annotations: template_annotation' list  *) }
[@@deriving yojson]

(* =============================================================================
   Program Section: template annotations
   ============================================================================= *)
and template_annotation = template_annotation' annotated [@@deriving yojson]

and template_annotation' =
  | IfElse of
      {condition: expr; then_branch: subprogram; else_branch: subprogram option}
  | Foreach of string annotated * expr * subprogram
[@@deriving yojson]

(* =============================================================================
   Program Section: Events
   ============================================================================= *)
and event = event' annotated [@@deriving yojson]

and event' =
  { info: event_info'
  ; io: event_io
  ; marking: event_marking (* ; annotations: template_annotation' list  *) }
[@@deriving yojson]

and event_id = string annotated [@@deriving yojson]

and event_label = string annotated [@@deriving yojson]

and event_info' = event_id * event_label [@@deriving yojson]

and event_io = event_io' annotated [@@deriving yojson]

and event_io' = Input of type_expr | Output of expr [@@deriving yojson]

and event_type' = InputType | OutputType [@@deriving yojson]

and event_marking = event_marking' annotated [@@deriving yojson]

and event_marking' =
  { executed: bool annotated
  ; pending: bool annotated
  ; included: bool annotated
  ; value: expr ref }
[@@deriving yojson]

(* =============================================================================
   Program Section: Relations
   ============================================================================= *)
and relation = relation' annotated [@@deriving yojson]

and relation' =
  | ControlRelation of event_id * expr * event_id * relation_type
  | SpawnRelation of event_id * expr * subprogram
[@@deriving yojson]

and relation_type = Condition | Include | Exclude | Milestone | Response
[@@deriving yojson]

(* =============================================================================
   Program Section: Type makers
   ============================================================================= *)

let annotate ?(loc = Nowhere) ?(ty = None) data = {data; loc; ty= ref ty}

let deannotate {data; _} = data

let deannotate_list lst = List.map deannotate lst

let mk_loc ?filename start_pos end_pos = Location (start_pos, end_pos, filename)

let mk_pos ?(filename = "") line column =
  {Lexing.dummy_pos with pos_fname= filename; pos_lnum= line; pos_cnum= column}

let mk_marking ?(executed = false) ?(pending = false) ?(included = true)
    ?(value = Unit) () =
  { executed= annotate ~ty:(Some BoolTy) executed
  ; pending= annotate ~ty:(Some BoolTy) pending
  ; included= annotate ~ty:(Some BoolTy) included
  ; value= ref @@ annotate value }

let default_marking = mk_marking ()

let default_marking_excl = mk_marking ~included:false ()

let default_marking_pend = mk_marking ~pending:true ()

let default_marking_pend_excl = mk_marking ~pending:true ~included:false ()

let mk_event ?(marking = default_marking) info io =
  annotate {info; io; marking= annotate marking}

let mk_ctrl_relation ~from ?(guard = annotate True) ~dest t =
  annotate @@ ControlRelation (from, guard, dest, t)

let mk_ctrl_relations left_ids expr right_ids t =
  List.concat_map
    (fun id1 ->
      List.map
        (fun id2 -> mk_ctrl_relation ~from:id1 ~guard:expr ~dest:id2 t)
        right_ids )
    left_ids

let mk_spawn_relation ~from ?(guard = annotate True) subprogram =
  annotate @@ SpawnRelation (from, guard, subprogram)

let mk_spawn_relations left_ids expr prog =
  List.map (fun id -> mk_spawn_relation ~from:id ~guard:expr prog) left_ids

let mk_program ?(template_decls = []) ?(events = []) ?(template_insts = [])
    ?(relations = []) ?(annotations = []) _ =
  {template_decls; events; template_insts; relations; annotations}

let to_program (events, insts, relations, annots) =
  mk_program ~events ~template_insts:insts ~relations ~annotations:annots ()

let mk_subprogram ?(events = []) ?(template_insts = []) ?(relations = [])
    ?(annotations = []) _ =
  (events, template_insts, relations, annotations)

let to_subprogram program =
  mk_subprogram ~events:program.events ~template_insts:program.template_insts
    ~relations:program.relations ~annotations:program.annotations ()

let empty_program = mk_program ()

let empty_subprogram = mk_subprogram ()

let mk_record fields =
  let fields = List.map (fun (k, v) -> (annotate k, annotate v)) fields in
  annotate (Record fields)

let get_event_type io =
  match io.data with Input _ -> InputType | Output _ -> OutputType

(* let empty_template_inst = mk_template_inst "" [] ~x:[] *)

(* =============================================================================
   Program Section: Pretty Printers
   ============================================================================= *)

let show_event_type' value_type event_type =
  let fmt =
    match event_type with
    | InputType -> Printf.sprintf "[?: %s]"
    | OutputType -> Printf.sprintf "[%s]"
  in
  fmt value_type

(* =============================================================================
   Program Section: Constants
   ============================================================================= *)

let trigger_id = "@trigger"
