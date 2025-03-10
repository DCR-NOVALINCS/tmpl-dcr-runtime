%{
open Ast.Syntax

type input = 
  | EventList of event list
  | TemplateInstList of template_instance list
  | RelationList of relation list
  | TemplateAnnotationList of template_annotation' list

let mk_program_from_top_level_input prog inputs = 
  List.fold_left (fun (events, template_insts, relations, annotations) -> function
    | EventList events' -> (events' @ events, template_insts, relations, annotations)
    | TemplateInstList template_insts' -> (events, template_insts' @ template_insts, relations, annotations)
    | RelationList relations' -> (events, template_insts, relations' @ relations, annotations)
    | TemplateAnnotationList annotations' -> (events, template_insts, relations, annotations' @ annotations)
  ) prog inputs
%}

%[@trace true]

// declarations
%token EOL
// type literals
%token <int> INT
%token <string> STR
// value literals
%token TRUE FALSE
%token <string> ID
// list maker
%token RANGE
// primitive types
%token UNITTY STRTY INTTY BOOLTY LISTTY
// delimiters
%token LPAR RPAR LBRACE RBRACE LBRACKET RBRACKET 
// separators
%token COMMA COLON SEMICOLON PIPE
// binary ops
%token PLUS MULT DIV AND OR EQ NEQ LESSTHAN LESSEQTHAN GREATERTHAN GREATEREQTHAN ASSIGN
// unary ops
%token NEG MINUS
// ====== DCR
// dcr literals
%token TRIGGER EXCL PEND
// (unguarded) dcr relations
%token INCLUDE EXCLUDE CONDITION RESPONSE CANCEL MILESTONE SPAWN
// (guarded) dcr relations - guard opening (left)
%token LGUARD LGUARD_RESPONSE
// (guarded) dcr relations - guard closing
%token RGUARD_INCLUDE RGUARD_EXCLUDE RGUARD_CONDITION RGUARD_RESPONSE RGUARD_CANCEL RGUARD_MILESTONE RGUARD_SPAWN
// information flow
// %token FLOWS TOP BOT
// templates
%token TEMPLATE
// conditional annotations
%token IF ELSE
// loop annotations
%token FOREACH IN
// misc
%token QUESTION PROP_DEREF BOLDARROW ARROW

%nonassoc NEG
%nonassoc PROP_DEREF 
%nonassoc LOWEST

%start main 
%start main_expr

%type <program> main
%type <expr> main_expr

%% /* rules */ 

main:
  plain_program EOL { $1 }
;

main_expr:
  expr EOL { $1 }
;

// program: mark_loc_ty(plain_program) {$1}
// plain_program:
//     template_decls = list(plain_template_decl);
//     spawn_prog = plain_program_spawn;
//     { 
//       let (events, template_insts, relations, annotations) = spawn_prog in
//       mk_program ~template_decls ~events ~template_insts ~relations ~annotations ()
//     }
// ;

// plain_program_spawn:
//   | input = list(plain_top_input); 
//     { 
//       mk_program_from_top_level_input input 
//     }

program: mark_loc_ty(plain_program) {$1}
plain_program:
    template_decls = terminated(list(plain_template_decl), SEMICOLON)?;
    spawn_prog = plain_program_spawn;
    { 
      let (events, template_insts, relations, annotations) = spawn_prog in
      mk_program ~template_decls:(Option.value ~default:[] template_decls) ~events ~template_insts ~relations ~annotations ()
    }
;

plain_program_spawn:
  | (* empty *) { mk_subprogram () }
  | plain_top_input SEMICOLON plain_program_spawn { mk_program_from_top_level_input $3 [$1] }
  // | plain_top_input { mk_program_from_top_level_input (mk_subprogram ()) [$1] }

plain_top_input:
  | plain_event_decl_list { EventList (List.rev $1) }
  | nonempty_list(template_inst) { TemplateInstList (List.rev $1) }
  | plain_ctrl_relation_decl_list { RelationList (List.rev $1) }
  | nonempty_list(plain_template_annotation) { TemplateAnnotationList (List.rev $1) }

// =====
// ===== template declaration 
// template_decl: mark_loc_ty(plain_template_decl) {$1}
plain_template_decl:
  | TEMPLATE; id = id; 
    params = delimited(LPAR, separated_list(COMMA, plain_template_param_pair), RPAR);
    (* TODO: Add types to the exported events *)
    export_types = preceded(COLON, separated_list(COMMA, id))?;
    graph = delimited(LBRACE, plain_program_spawn, RBRACE);
    export = preceded(BOLDARROW, separated_nonempty_list(COMMA, id))?;
    { { id; params; export_types = Option.value ~default:[] export_types; graph; export = Option.value ~default:[] export } }

plain_template_param_pair:
  | id=id; COLON; ty=type_expr; default_expr=preceded(ASSIGN, expr)?;           { (id, ty, default_expr) }
  // | id=id; COLON; label=id                                                      { (id, EventParam(label)) }

// ===== template instantiation
template_inst: mark_loc_ty(plain_template_inst) {$1}
plain_template_inst:
  | tmpl_id = id;
    args = delimited(LPAR, separated_list(COMMA, plain_arg_pair), RPAR);
    x = preceded(BOLDARROW, separated_nonempty_list(COMMA, id))?;
    // tmpl_annotations = preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
    {
      {
        tmpl_id
        ; args
        ; x = Option.value ~default:[] x
        (*; tmpl_annotations = Option.value ~default:[] tmpl_annotations*)
      }
    }

plain_arg_pair:
  // | id=id; ARROW; event_id=id { (id, EventArg event_id) }
  | id=id; ASSIGN; expr=expr { (id, expr) }

// template_annotations: 
//   | annotations = separated_list(PIPE, plain_template_annotation) { annotations }

// template annotations 
plain_template_annotation:
  // Loop annotation 
  | FOREACH; id=id; IN; l=expr;                                                 
    graph = delimited(LBRACE, plain_program_spawn, RBRACE);
  { Foreach (id, l, graph) }
  // Conditional annotation
  | IF; condition = expr;
    then_branch = delimited(LBRACE, plain_program_spawn, RBRACE);
    else_branch = preceded(ELSE, delimited(LBRACE, plain_program_spawn, RBRACE))?;
  { IfElse { condition; then_branch; else_branch } }

// =====
// ===== event declarations
/* event_decl_list: mark_loc_ty(plain_event_decl_list) {$1} */
plain_event_decl_list:
  | events = nonempty_list(event_decl) { events }

event_decl: mark_loc_ty(plain_event_decl) {$1}
plain_event_decl:
  // event has marking as prefix (one of !, %, !%, %!)
  | marking = marking_prefix?;
    info = delimited(LPAR, plain_event_info, RPAR); 
    io = delimited(LBRACKET, event_io, RBRACKET);
    // annotations = preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
    { 
      { marking = (Option.value ~default:(annotate default_marking) marking)
      ; info
      ; io
      (*; annotations = Option.value ~default:[] annotations*)
      } 
    }
  // (optionally) event has marking after the input/output expression
  | info = delimited(LPAR, plain_event_info, RPAR); 
    io = delimited(LBRACKET, event_io, RBRACKET);
    marking = delimited(LBRACE, node_marking, RBRACE)?;
    // annotations = preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
    { 
      { marking = (Option.value ~default:(annotate default_marking) marking)
      ; info
      ; io
      (*; annotations = Option.value ~default:[] annotations*)
      } 
    }
;

marking_prefix: mark_loc_ty(plain_marking_prefix) {$1}
plain_marking_prefix:
  | EXCL                                                                        { default_marking_excl }
  | PEND                                                                        { default_marking_pend }
  | EXCL PEND | PEND EXCL                                                       { default_marking_pend_excl }
;

// event_info: mark_loc_ty(plain_event_info) {$1}
plain_event_info:
  | separated_pair(id, COLON, id)                                               { $1 }
;

// =====
// ===== control relation declarations
// ctrl_relation_decl:list: mark_loc_ty(plain_ctrl_relation_decl_list) {$1}
plain_ctrl_relation_decl_list:
  | rels=nonempty_list(plain_group_ctrl_relation_decl);                         { List.flatten rels }


// group_ctrl_relation_decl: mark_loc_ty(plain_group_ctrl_relation_decl) {$1}
plain_group_ctrl_relation_decl:
  // ==== Unguarded relations ====
  // Include
  | left_ids=separated_nonempty_list(COMMA, id);
  INCLUDE; 
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids (annotate (BoolLit true)) right_ids Include }

  // Exclude
  | left_ids=separated_nonempty_list(COMMA, id);
  EXCLUDE;
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids (annotate (BoolLit true)) right_ids Exclude }

  // Condition
  | left_ids=separated_nonempty_list(COMMA, id);
  CONDITION;
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids (annotate (BoolLit true)) right_ids Condition }

  // Response
  | left_ids=separated_nonempty_list(COMMA, id);
  RESPONSE;
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids (annotate (BoolLit true)) right_ids Response }

  // Cancel
  | left_ids=separated_nonempty_list(COMMA, id);
  CANCEL;
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids (annotate (BoolLit true)) right_ids Cancel }

  // Milestone
  | left_ids=separated_nonempty_list(COMMA, id);
  MILESTONE;
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids (annotate (BoolLit true)) right_ids Milestone }

  // Spawn 
  | left_ids=separated_nonempty_list(COMMA, id); 
  SPAWN; 
  prog=delimited(LBRACE, plain_program_spawn, RBRACE);
  { mk_spawn_relations left_ids (annotate (BoolLit true)) prog }

  | left_ids=separated_nonempty_list(COMMA, id);
  SPAWN;
  inst=template_inst;
  { mk_spawn_relations left_ids (annotate (BoolLit true)) (mk_subprogram ~template_insts:[inst] ()) }

  // ==== Guarded relations ==== 
  // Include
  | left_ids=separated_nonempty_list(COMMA, id);
  expr=delimited(LGUARD, expr, RGUARD_INCLUDE);
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids expr right_ids Include }

  // Exclude
  | left_ids=separated_nonempty_list(COMMA, id);
  expr=delimited(LGUARD, expr, RGUARD_EXCLUDE);
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids expr right_ids Exclude }

  // Condition
  | left_ids=separated_nonempty_list(COMMA, id);
  expr=delimited(LGUARD, expr, RGUARD_CONDITION);
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids expr right_ids Condition }

  // Response
  | left_ids=separated_nonempty_list(COMMA, id);
  expr=delimited(LGUARD_RESPONSE, expr, RGUARD_RESPONSE);
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids expr right_ids Response }

  // Cancel
  | left_ids=separated_nonempty_list(COMMA, id);
  expr=delimited(LGUARD_RESPONSE, expr, RGUARD_CANCEL);
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids expr right_ids Cancel }
  
  // Milestone
  | left_ids=separated_nonempty_list(COMMA, id);
  expr=delimited(LGUARD, expr, RGUARD_MILESTONE);
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids expr right_ids Milestone }

  // Spawn
  | left_ids=separated_nonempty_list(COMMA, id);
  expr=delimited(LGUARD, expr, RGUARD_SPAWN);
  prog=delimited(LBRACE, plain_program_spawn, RBRACE);
  { mk_spawn_relations left_ids expr prog }

  | left_ids=separated_nonempty_list(COMMA, id);
  expr=delimited(LGUARD, expr, RGUARD_SPAWN);
  inst=template_inst;
  { mk_spawn_relations left_ids expr (mk_subprogram ~template_insts:[inst] ()) }
  ;


event_io: mark_loc_ty(plain_event_io) {$1}
plain_event_io:
| input_type = preceded(QUESTION, input_expr)                                   { Input(input_type) }
| computationExpr = expr                                                        { Output(computationExpr) }
;

input_expr: mark_loc_ty(plain_input_expr) { $1 }
plain_input_expr:
  |                                                                             { UnitTy }
  | COLON; ty = plain_type_expr                                                 { ty }
;

node_marking: mark_loc_ty(plain_node_marking) {$1}
plain_node_marking:
  | exec=bool; COMMA; pend=bool; COMMA; inc=bool                                
  { 
    { executed = exec
    ; pending = pend
    ; included = inc
    ; value = ref @@ annotate Unit
    } 
  }
;


type_expr: mark_loc_ty(plain_type_expr) {$1}
plain_type_expr:
| UNITTY                                                                        { UnitTy  }
| STRTY                                                                         { StringTy }
| INTTY                                                                         { IntTy    }
| BOOLTY                                                                        { BoolTy   }
| id                                                                            { EventTy($1) }
| delimited(LBRACE, plain_record_type_field_list, RBRACE)                       { RecordTy($1) }
| LISTTY; item_type=delimited(LBRACKET, plain_type_expr, RBRACKET)              { ListTy(item_type) }
;

// ================= expressions

expr: mark_loc_ty(plain_expr) { $1 }
plain_expr:
| plain_range                                                                   { $1 }
| plain_orop                                                                    { $1 }
;

// range: mark_loc_ty(plain_range) { $1 }
plain_range:
| RANGE; LPAR; start_expr = arith; COMMA; end_expr = arith; RPAR;               { Range(start_expr, end_expr) }

orop: mark_loc_ty(plain_orop) { $1 }
plain_orop:
| orop OR andop                                                                 { BinaryOp($1, $3, Or) }
| plain_andop                                                                   { $1 }
;

andop: mark_loc_ty(plain_andop) { $1 }
plain_andop:
| andop AND compareop                                                           { BinaryOp($1, $3, And) }
| plain_compareop                                                               { $1 }
;

compareop: mark_loc_ty(plain_compareop) { $1 }
plain_compareop:
| compareop EQ arith                                                            { BinaryOp($1, $3, Eq) }
| compareop NEQ arith                                                           { BinaryOp($1, $3, NotEq) }
| compareop GREATERTHAN arith                                                   { BinaryOp($1, $3, GreaterThan) }
| compareop GREATEREQTHAN arith                                                 { BinaryOp($1, $3, GreaterOrEqual) }
| compareop LESSTHAN arith                                                      { BinaryOp($1, $3, LessThan) }
| compareop LESSEQTHAN arith                                                    { BinaryOp($1, $3, LessOrEqual) }
| plain_arith                                                                   { $1 } 
;

arith: mark_loc_ty(plain_arith) { $1 }
plain_arith: 
| arith PLUS term                                                               { BinaryOp($1, $3, Add) }
| arith MINUS term                                                              { BinaryOp($1, $3, Sub) }
| plain_term                                                                    { $1          }
;

term: mark_loc_ty(plain_term) { $1 }
plain_term: 
| term MULT fact                                                                { BinaryOp($1, $3, Mult) }
| term DIV fact                                                                 { BinaryOp($1, $3, Div) }
| plain_fact                                                                    { $1 }
;

fact: mark_loc_ty(plain_fact) { $1 }
plain_fact:
| plain_property_deref                                                          { $1 }
| TRUE                                                                          { BoolLit true }
| FALSE                                                                         { BoolLit false }
| plain_integer                                                                 { IntLit($1) }
| STR                                                                           { StringLit($1) }
| id                                                                            { Identifier($1) } 
| TRIGGER                                                                       { Trigger }
| plain_record                                                                  { Record($1) }
| NEG fact                                                                      { UnaryOp($2, Negation) }
// | MINUS fact                                                                    { UnaryOp($2, Minus) }
// | expr = fact; PROP_DEREF; prop = id;                                           { PropDeref(expr, prop) }
| expr = delimited(LPAR, expr, RPAR)                                            { Parenthesized(expr) }
| list = delimited(LBRACKET, separated_list(COMMA, expr), RBRACKET)             { List(list) }
// | RANGE; LPAR; start_expr = expr; COMMA; end_expr = expr; RPAR                  { Range(start_expr, end_expr) }
;

property_deref: mark_loc_ty(plain_property_deref) { $1 }
plain_property_deref:
| r=fact PROP_DEREF p=id                                                        { PropDeref(r, p) }

bool: mark_loc_ty(plain_bool) { $1 }
plain_bool:
  | TRUE        { true }
  | FALSE       { false }
;

integer: mark_loc_ty(plain_integer) { $1 }
plain_integer:
| INT                                                                           { $1 }
| MINUS INT                                                                     { -$2 }


// ============== records
/* record: mark_loc_ty(plain_record) { $1 } */
plain_record:
| record = delimited(LBRACE, plain_record_field_list, RBRACE)                   { record }
;

/* record_field_list: mark_loc_ty(plain_record_field_list) { $1 } */
plain_record_field_list:
| fields = separated_nonempty_list(COMMA, plain_record_field(expr))             { fields }
;

// record_type_field_list: mark_loc_ty(plain_record_type_field_list) { $1 }
plain_record_type_field_list:
  separated_nonempty_list(COMMA, plain_record_field(type_expr))                 { $1 } 
;

plain_record_field(X):
| name=id; COLON; value=X                                                       {(name, value)}

// === the rule id 
id: mark_loc_ty(plain_id) {$1}
plain_id:
  | id=ID  { id }

name: mark_loc_ty(plain_name) {$1}
plain_name:
  | name=STR  { name }

mark_loc_ty(X):
  x = X
  { annotate ~loc:(Location($startpos, $endpos, if not ($startpos.pos_fname = "") then (Some $startpos.pos_fname) else None)) x}
;
%%
