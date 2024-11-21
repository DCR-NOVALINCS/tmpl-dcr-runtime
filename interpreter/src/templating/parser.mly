%{
open Syntax

type top_level_input = 
  (*| TemplateDef of template_def*)
  | Event of event
  | TemplateInst of template_instance
  | Relations of relation list

let mk_program_from_top_level_input =
  List.fold_left (fun program x -> match x with
    (* | TemplateDef x -> { program with template_decls = x :: program.template_decls }*)
    | Event event -> { program with events = event :: program.events }
    | TemplateInst inst -> { program with template_insts = inst :: program.template_insts }
    | Relations relations -> { program with relations = List.append relations program.relations }
  ) empty_program

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
// primitive types
%token STRTY INTTY BOOLTY LISTTY
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
%token INCLUDE EXCLUDE CONDITION RESPONSE MILESTONE SPAWN
// (guarded) dcr relations - guard opening (left)
%token LGUARD LGUARD_RESPONSE
// (guarded) dcr relations - guard closing
%token RGUARD_INCLUDE RGUARD_EXCLUDE RGUARD_CONDITION RGUARD_RESPONSE RGUARD_MILESTONE RGUARD_SPAWN
// information flow
// %token FLOWS TOP BOT
// templates
%token TEMPLATE
%token FOREACH WHEN IN
// misc
%token QUESTION PROP_DEREF BOLDARROW ARROW

%nonassoc NEG
%nonassoc PROP_DEREF 

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
plain_program:
    template_decls = list(plain_template_decl);
    spawn_prog = plain_program_spawn;
    { 
      let (events, template_insts, relations) = spawn_prog in
      {
        template_decls
        ; events
        ; template_insts
        ; relations
      } 
    }
;

/* program_spawn: mark_loc_ty(plain_program_spawn) {$1} */
// plain_program_spawn:
//     events = plain_event_decl_list;
//     template_insts = preceded(SEMICOLON, nonempty_list(plain_template_inst))?;
//     relations = preceded(SEMICOLON, plain_ctrl_relation_decl_list)?;
//     { (
//         events, 
//         Option.value ~default:[] template_insts, 
//         Option.value ~default:[] relations
//       ) } 
// ;

plain_program_spawn:
  | input = list(plain_top_input); 
    { let { events; template_insts; relations; _ } = mk_program_from_top_level_input input in
    (events, template_insts, relations) }


plain_top_input:
  // | plain_template_decl { TemplateDef($1) }
  | event_decl { Event($1) }
  | template_inst { TemplateInst($1) }
  | plain_ctrl_relation_decl_list { Relations($1) }

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
  | id=id; COLON; ty=type_expr; default_expr=preceded(ASSIGN, expr)?; { (id, ExprParam(ty, default_expr)) }
  | id=id; COLON; label=id { (id, EventParam(label)) }

// ===== template instantiation
template_inst: mark_loc_ty(plain_template_inst) {$1}
plain_template_inst:
  | tmpl_id = id;
    args = delimited(LPAR, separated_list(COMMA, plain_arg_pair), RPAR);
    x = preceded(BOLDARROW, separated_nonempty_list(COMMA, id))?;
    tmpl_annotations = preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
    {
      {
        tmpl_id
        ; args
        ; x = Option.value ~default:[] x
        ; tmpl_annotations = Option.value ~default:[] tmpl_annotations
      }
    }

plain_arg_pair:
  | id=id; ARROW; event_id=id { (id, EventArg event_id) }
  | id=id; ASSIGN; expr=expr { (id, ExprArg expr) }

// template annotations 
plain_template_annotation:
  | WHEN; expr = expr { When(expr) }
  | FOREACH; id=id; IN; l=expr { Foreach (id, l) }

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
    annotations = preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
    { 
      { marking = (Option.value ~default:(annotate default_marking) marking)
      ; info
      ; io
      ; annotations = Option.value ~default:[] annotations
      } 
    }
  // (optionally) event has marking after the input/output expression
  | info = delimited(LPAR, plain_event_info, RPAR); 
    io = delimited(LBRACKET, event_io, RBRACKET);
    marking = delimited(LBRACE, node_marking, RBRACE)?;
    annotations = preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
    { 
      { marking = (Option.value ~default:(annotate default_marking) marking)
      ; info
      ; io
      ; annotations = Option.value ~default:[] annotations
      } 
    }
;

marking_prefix: mark_loc_ty(plain_marking_prefix) {$1}
plain_marking_prefix:
  | EXCL                      { default_marking_excl }
  | PEND                      { default_marking_pend }
  | EXCL PEND | PEND EXCL     { default_marking_pend_excl }
;

// event_info: mark_loc_ty(plain_event_info) {$1}
plain_event_info:
  | separated_pair(id, COLON, id)    { $1 }
;

// =====
// ===== control relation declarations
// ctrl_relation_decl:list: mark_loc_ty(plain_ctrl_relation_decl_list) {$1}
plain_ctrl_relation_decl_list:
  | rels=nonempty_list(plain_group_ctrl_relation_decl);
  { List.flatten rels }


//FIXME: Repetition of the template annotations in the plain_group_ctrl_relation_decl
// group_ctrl_relation_decl: mark_loc_ty(plain_group_ctrl_relation_decl) {$1}
plain_group_ctrl_relation_decl:
  // ==== Unguarded relations ====
  // Include
  | left_ids=separated_nonempty_list(COMMA, id);
  INCLUDE; 
  right_ids=separated_nonempty_list(COMMA, id);
  annotations=preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
  { mk_ctrl_relations left_ids (annotate True) right_ids ~annotations:(Option.value ~default:[] annotations) Include }

  // Exclude
  | left_ids=separated_nonempty_list(COMMA, id);
  EXCLUDE;
  right_ids=separated_nonempty_list(COMMA, id);
  annotations=preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
  { mk_ctrl_relations left_ids (annotate True) right_ids ~annotations:(Option.value ~default:[] annotations) Exclude }

  // Condition
  | left_ids=separated_nonempty_list(COMMA, id);
  CONDITION;
  right_ids=separated_nonempty_list(COMMA, id);
  annotations=preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
  { mk_ctrl_relations left_ids (annotate True) right_ids ~annotations:(Option.value ~default:[] annotations) Condition }

  // Response
  | left_ids=separated_nonempty_list(COMMA, id);
  RESPONSE;
  right_ids=separated_nonempty_list(COMMA, id);
  annotations=preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
  { mk_ctrl_relations left_ids (annotate True) right_ids ~annotations:(Option.value ~default:[] annotations) Response }

  // Milestone
  | left_ids=separated_nonempty_list(COMMA, id);
  MILESTONE;
  right_ids=separated_nonempty_list(COMMA, id);
  annotations=preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
  { mk_ctrl_relations left_ids (annotate True) right_ids ~annotations:(Option.value ~default:[] annotations) Milestone }

  // Spawn 
  | left_ids=separated_nonempty_list(COMMA, id); 
  SPAWN; 
  prog=delimited(LBRACE, plain_program_spawn, RBRACE);
  annotations=preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;   
  { mk_spawn_relations left_ids (annotate True) ~annotations:(Option.value ~default:[] annotations) prog }

  // ==== Guarded relations ==== 
  // Include
  | left_ids=separated_nonempty_list(COMMA, id);
  expr=delimited(LGUARD, expr, RGUARD_INCLUDE);
  right_ids=separated_nonempty_list(COMMA, id);
  annotations=preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
  { mk_ctrl_relations left_ids expr right_ids ~annotations:(Option.value ~default:[] annotations) Include }

  // Exclude
  | left_ids=separated_nonempty_list(COMMA, id);
  expr=delimited(LGUARD, expr, RGUARD_EXCLUDE);
  right_ids=separated_nonempty_list(COMMA, id);
  annotations=preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
  { mk_ctrl_relations left_ids expr right_ids ~annotations:(Option.value ~default:[] annotations) Exclude }

  // Condition
  | left_ids=separated_nonempty_list(COMMA, id);
  expr=delimited(LGUARD, expr, RGUARD_CONDITION);
  right_ids=separated_nonempty_list(COMMA, id);
  annotations=preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
  { mk_ctrl_relations left_ids expr right_ids ~annotations:(Option.value ~default:[] annotations) Condition }

  // Response
  | left_ids=separated_nonempty_list(COMMA, id);
  expr=delimited(LGUARD_RESPONSE, expr, RGUARD_RESPONSE);
  right_ids=separated_nonempty_list(COMMA, id);
  annotations=preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
  { mk_ctrl_relations left_ids expr right_ids ~annotations:(Option.value ~default:[] annotations) Response }

  // Milestone
  | left_ids=separated_nonempty_list(COMMA, id);
  expr=delimited(LGUARD, expr, RGUARD_MILESTONE);
  right_ids=separated_nonempty_list(COMMA, id);
  annotations=preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
  { mk_ctrl_relations left_ids expr right_ids ~annotations:(Option.value ~default:[] annotations) Milestone }

  // Spawn
  | left_ids=separated_nonempty_list(COMMA, id);
  expr=delimited(LGUARD, expr, RGUARD_SPAWN);
  prog=delimited(LBRACE, plain_program_spawn, RBRACE);
  annotations=preceded(MINUS, separated_list(PIPE, plain_template_annotation))?;
  { mk_spawn_relations left_ids expr ~annotations:(Option.value ~default:[] annotations) prog }


event_io: mark_loc_ty(plain_event_io) {$1}
plain_event_io:
| input_type = preceded(QUESTION, input_expr)           { Input(input_type) }
| computationExpr = expr                    { Output(computationExpr) }
;

input_expr: mark_loc_ty(plain_input_expr) { $1 }
plain_input_expr:
  |                                      { UnitTy }
  | ty = preceded(COLON, plain_type_expr)      { ty }
;

node_marking: mark_loc_ty(plain_node_marking) {$1}
plain_node_marking:
  | exec=bool; COMMA; pend=bool; COMMA; inc=bool        { {executed = exec; pending = pend; included = inc; value = ref @@ annotate Unit} }
;


type_expr: mark_loc_ty(plain_type_expr) {$1}
plain_type_expr:
| STRTY                                             { StringTy }
| INTTY                                             { IntTy    }
| BOOLTY                                            { BoolTy   }
// | plain_id                                          { EventTy($1) }
| delimited(LBRACE, plain_record_type_field_list, RBRACE)    { RecordTy($1) }
| LISTTY; item_type=delimited(LBRACKET, plain_type_expr, RBRACKET)             { ListTy(item_type) }
;

// ================= expressions

expr: mark_loc_ty(plain_expr) { $1 }
plain_expr:
|  plain_orop                                       { $1 }
;

orop: mark_loc_ty(plain_orop) { $1 }
plain_orop:
| orop OR andop                                     { BinaryOp($1,$3, Or) }
| plain_andop                                       { $1 }
;

andop: mark_loc_ty(plain_andop) { $1 }
plain_andop:
| andop AND compareop                               { BinaryOp($1,$3,And) }
| plain_compareop                                   { $1 }
;

compareop: mark_loc_ty(plain_compareop) { $1 }
plain_compareop:
| compareop EQ arith                                { BinaryOp($1,$3,Eq) }
| compareop NEQ arith                               { BinaryOp($1,$3,NotEq) }
| compareop GREATERTHAN arith                       { BinaryOp($1,$3,GreaterThan) }
| compareop GREATEREQTHAN arith                     { BinaryOp($1,$3,GreaterOrEqual) }
| compareop LESSTHAN arith                          { BinaryOp($1,$3,LessThan) }
| compareop LESSEQTHAN arith                          { BinaryOp($1,$3,LessOrEqual) }
| plain_arith                                       { $1 } 
;

arith: mark_loc_ty(plain_arith) { $1 }
plain_arith: 
| arith PLUS term                                   { BinaryOp($1,$3,Add) }
| arith MINUS term                                  { BinaryOp($1,$3,Sub) }
| plain_term                                        { $1          }
;

term: mark_loc_ty(plain_term) { $1 }
plain_term: 
| term MULT fact                                                    { BinaryOp($1,$3,Mult) }
| term DIV fact                                                     { BinaryOp($1,$3,Div) }
| plain_fact                                                        { $1 }
;

fact: mark_loc_ty(plain_fact) { $1 }
plain_fact:
| TRUE                                                              { True }
| FALSE                                                             { False }
| INT                                                               { IntLit($1) }
| STR                                                               { StringLit($1) }
| id                                                                { Identifier($1) } 
| TRIGGER                                                           { Trigger }
| v = plain_record                                                  { Record(v) }
| expr = preceded(NEG, fact)                                        { UnaryOp(expr,Negation) }
| MINUS fact                                                        { UnaryOp($2, Minus) }
| expr = fact; PROP_DEREF; prop = id;                               { PropDeref(expr, prop) }
| expr = delimited(LPAR, plain_expr, RPAR)                                { expr }
| list = delimited(LBRACKET, separated_list(COMMA, expr), RBRACKET)                        { List(list) }
// | LBRACKET; start_expr= expr; PROP_DEREF;PROP_DEREF; end_expr=expr; RBRACKET; { Range(start_expr, end_expr) }
;

bool: mark_loc_ty(plain_bool) { $1 }
plain_bool:
  | TRUE        { true }
  | FALSE       { false }
;


// ============== records
/* record: mark_loc_ty(plain_record) { $1 } */
plain_record:
| record = delimited(LBRACE, plain_record_field_list, RBRACE) { record }
;

/* record_field_list: mark_loc_ty(plain_record_field_list) { $1 } */
plain_record_field_list:
| fields = separated_nonempty_list(COMMA, plain_record_field(expr)) { fields }
;

/* record_field: mark_loc_ty(plain_record_field) { $1 } */
// plain_record_field:
// | name=id; COLON; value=expr               {(name, value)}
// ;

// record_type_field_list: mark_loc_ty(plain_record_type_field_list) { $1 }
plain_record_type_field_list:
  separated_nonempty_list(COMMA, plain_record_field(type_expr)) { $1 } 
;

// record_field_type: mark_loc_ty(plain_record_type_field) {$1}
// plain_record_type_field:
// | name=id; COLON; value=type_expr     {(name, value)}
// ;

plain_record_field(X):
| name=id; COLON; value=X               {(name, value)}

// === the rule id 
id: mark_loc_ty(plain_id) {$1}
plain_id:
  | id=ID  { id }


mark_loc_ty(X):
  x = X
  { annotate ~loc:(Location($startpos, $endpos, if not ($startpos.pos_fname = "") then (Some $startpos.pos_fname) else None)) x}
;
%%
