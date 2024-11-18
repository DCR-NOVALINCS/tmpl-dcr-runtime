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
  | ListTy of type_expr
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
   Program
   ============================================================================= *)
and program =
  { template_decls: template_def list
  ; events: event list
  ; template_insts: template_instance list
  ; relations: relation list }
[@@deriving yojson]

and subprogram = event list * template_instance list * relation list
[@@deriving yojson]

(* =============================================================================
   Program Section: template definitions
   ============================================================================= *)
and template_param_type = template_param_type' annotated [@@deriving yojson]

and template_param_type' =
  | ExprParam of type_expr * expr option
  | EventParam of event_info'
[@@deriving yojson]

and template_param' = string annotated * template_param_type [@@deriving yojson]

and template_def =
  { export: event_id list
  ; params: (string annotated * type_expr * expr option) list
  ; export_types: event_label list
  ; graph: subprogram
  ; id: string annotated }
[@@deriving yojson]

(* =============================================================================
   Program Section: template instantiations
   ============================================================================= *)
and template_arg_type = ExprArg of expr | EventArg of event_id
[@@deriving yojson]

and template_arg = string annotated * template_arg_type [@@deriving yojson]

and template_instance = template_instance' annotated [@@deriving yojson]

and template_instance' =
  { args: (string annotated * expr) list
  ; x: event_id list
  ; tmpl_id: string annotated
  ; tmpl_annotations: template_annotation' list }
[@@deriving yojson]

(* =============================================================================
   Program Section: template annotations
   ============================================================================= *)
and template_annotation = template_annotation' annotated [@@deriving yojson]

and template_annotation' = When of expr | Foreach of string annotated * expr
[@@deriving yojson]

(* =============================================================================
   Program Section: Events
   ============================================================================= *)
and event = event' annotated [@@deriving yojson]

and event' =
  { info: event_info'
  ; io: event_io
  ; marking: event_marking
  ; annotations: template_annotation' list }
[@@deriving yojson]

and event_id = string annotated [@@deriving yojson]

and event_label = string annotated [@@deriving yojson]

and event_info' = event_id * event_label [@@deriving yojson]

and event_io = event_io' annotated [@@deriving yojson]

and event_io' = Input of type_expr | Output of expr [@@deriving yojson]

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
  | ControlRelation of
      event_id * expr * event_id * relation_type * template_annotation' list
  | SpawnRelation of event_id * expr * subprogram * template_annotation' list
[@@deriving yojson]

and relation_type = Condition | Include | Exclude | Milestone | Response
[@@deriving yojson]

(* =============================================================================
   Program Section: Type makers
   ============================================================================= *)

let annotate ?(loc = Nowhere) ?(ty = None) data = {data; loc; ty= ref ty}

let deannotate {data; _} = data

let deannotate_list lst = List.map deannotate lst

let mk_marking ?(executed = false) ?(pending = false) ?(included = true)
    ?(value = Unit) () =
  { executed= annotate executed
  ; pending= annotate pending
  ; included= annotate included
  ; value= ref @@ annotate value }

let default_marking = mk_marking ()

let default_marking_excl = mk_marking ~included:false ()

let default_marking_pend = mk_marking ~pending:true ()

let default_marking_pend_excl = mk_marking ~pending:true ~included:false ()

let mk_event ?(marking = default_marking) ?(annotations = []) info io =
  annotate {info; io; marking= annotate marking; annotations}

let mk_ctrl_relation ?(annotations = []) ~from ?(guard = annotate True) ~dest t
    =
  annotate @@ ControlRelation (from, guard, dest, t, annotations)

let mk_ctrl_relations ?(annotations = []) left_ids expr right_ids t =
  List.concat_map
    (fun id1 ->
      List.map
        (fun id2 ->
          mk_ctrl_relation ~from:id1 ~guard:expr ~dest:id2 ~annotations t )
        right_ids )
    left_ids

let mk_spawn_relation ?(annotations = []) ~from ?(guard = annotate True)
    subprogram =
  annotate @@ SpawnRelation (from, guard, subprogram, annotations)

let mk_spawn_relations ?(annotations = []) left_ids expr prog =
  List.map
    (fun id -> mk_spawn_relation ~from:id ~guard:expr ~annotations prog)
    left_ids

let mk_program ?(template_decls = []) ?(events = []) ?(template_insts = [])
    ?(relations = []) _ =
  {template_decls; events; template_insts; relations}

let mk_subprogram ?(events = []) ?(template_insts = []) ?(relations = []) _ =
  (events, template_insts, relations)

let empty_program = mk_program ()

let empty_subprogram = mk_subprogram ()

(* let empty_template_inst = mk_template_inst "" [] ~x:[] *)

(* =============================================================================
   Program Section: Pretty Printers
   ============================================================================= *)
(* let string_of_pos pos = let line = pos.Lexing.pos_lnum in let start_char =
   pos.Lexing.pos_cnum - pos.Lexing.pos_bol in Printf.sprintf "%d:%d" line
   start_char

   let string_of_loc loc = match loc with | Nowhere -> "?" | Location
   (start_pos, end_pos, filename) -> let filename = Option.value filename
   ~default:"" in let start_pos_string = string_of_pos start_pos in let
   end_pos_string = string_of_pos end_pos in Printf.sprintf "%s:%s:%s" filename
   start_pos_string end_pos_string *)

(* =============================================================================
   Alpha-renaming functions
   ============================================================================= *)
