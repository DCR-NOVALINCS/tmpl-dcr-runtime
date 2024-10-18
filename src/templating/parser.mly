%{
open Syntax
%}

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
%token PLUS MULT AND OR EQ NEQ LESSTHAN GREATERTHAN
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
// misc
%token QUESTION PROP_DEREF BOLDARROW ARROW
// %token PIPE // TODO revise utility
// %token UDRSCR // TODO revise utility

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
    template_decls = terminated(list(plain_template_decl), SEMICOLON)?;
    spawn_prog = plain_program_spawn;
    { 
      let (events, template_insts, relations) = spawn_prog in
      {
        template_decls = Option.value ~default:[] template_decls
        ; events
        ; template_insts
        ; relations
      } 
    }
;

/* program_spawn: mark_loc_ty(plain_program_spawn) {$1} */
plain_program_spawn:
    events = plain_event_decl_list;
    template_insts = terminated(list(plain_template_inst), SEMICOLON)?;
    relations = preceded(SEMICOLON, plain_ctrl_relation_decl_list)?;
    { (events, Option.value ~default:[] template_insts, Option.value ~default:[] relations) } 
;

// =====
// ===== template declaration 
// template_decl: mark_loc_ty(plain_template_decl) {$1}
plain_template_decl:
  | TEMPLATE; id = id; 
    params=delimited(LPAR, separated_list(COMMA, pair(id, type_expr)), RPAR);
    (* TODO: Add types to the exported events *)
    graph= delimited(LBRACE, plain_program_spawn, RBRACE);
    export=preceded(BOLDARROW, separated_nonempty_list(COMMA, id))?;
    { { id; params; graph; export=Option.value ~default:[] export } }

// ===== template instantiation
plain_template_inst:
  | tmpl_id = id;
    args = delimited(LPAR, separated_list(COMMA, pair(id, expr)), RPAR);
    x = preceded(BOLDARROW, separated_nonempty_list(COMMA, id))?;
    {
      {
        tmpl_id
        ; args
        ; x=Option.value ~default:[] x
        ; tmpl_annotations = []
      }
    }


// =====
// ===== event declarations
/* event_decl_list: mark_loc_ty(plain_event_decl_list) {$1} */
plain_event_decl_list:
  | events = nonempty_list(event_decl) { events }

event_decl: mark_loc_ty(plain_event_decl) {$1}
plain_event_decl:
  // event has marking as prefix (one of !, %, !%, %!)
  | marking = marking_prefix?;
    info = delimited(LPAR, plain_event_info , RPAR); 
    io = delimited(LBRACKET, event_io, RBRACKET);
    { { marking = (Option.value ~default:(annotate default_marking) marking); info; io; annotations = [] } }
  // (optionally) event has marking after the input/output expression
  | info = delimited(LPAR, plain_event_info , RPAR); 
    io = delimited(LBRACKET, event_io, RBRACKET);
    marking = delimited(LBRACE, node_marking, RBRACE)?;
    { { marking = (Option.value ~default:(annotate default_marking) marking); info; io; annotations = [] } }
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
  | rels=nonempty_list(plain_group_ctrl_relation_decl) { List.flatten rels }


// group_ctrl_relation_decl: mark_loc_ty(plain_group_ctrl_relation_decl) {$1}
plain_group_ctrl_relation_decl:
  // ==== Unguarded relations ====
  // Include
  | left_ids=separated_nonempty_list(COMMA, id);
  INCLUDE; 
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids (annotate True) right_ids Include }

  // Exclude
  | left_ids=separated_nonempty_list(COMMA, id);
  EXCLUDE;
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids (annotate True) right_ids Exclude }

  // Condition
  | left_ids=separated_nonempty_list(COMMA, id);
  CONDITION;
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids (annotate True) right_ids Condition }

  // Response
  | left_ids=separated_nonempty_list(COMMA, id);
  RESPONSE;
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids (annotate True) right_ids Response }

  // Milestone
  | left_ids=separated_nonempty_list(COMMA, id);
  MILESTONE;
  right_ids=separated_nonempty_list(COMMA, id);
  { mk_ctrl_relations left_ids (annotate True) right_ids Milestone }

  // Spawn 
  | left_ids=separated_nonempty_list(COMMA, id); 
  SPAWN; 
  prog=delimited(LBRACE, plain_program_spawn, RBRACE);   
  { mk_spawn_relations left_ids (annotate True) prog }

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
  | exec=bool; COMMA; pend=bool; COMMA; inc=bool        { {executed = exec; pending = pend; included = inc; value = annotate Unit} }
;


type_expr: mark_loc_ty(plain_type_expr) {$1}
plain_type_expr:
| STRTY                                             { StringTy }
| INTTY                                             { IntTy    }
| BOOLTY                                            { BoolTy   }
| plain_id                                          { EventTy($1) }
| delimited(LBRACE, plain_record_type_field_list, RBRACE)    { RecordTy($1) }
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
| compareop LESSTHAN arith                          { BinaryOp($1,$3,LessThan) }
| plain_arith                                       { $1 } 
;

arith: mark_loc_ty(plain_arith) { $1 }
plain_arith: 
| arith PLUS term                                   { BinaryOp($1,$3,Add) }
| plain_term                                        { $1          }
;

term: mark_loc_ty(plain_term) { $1 }
plain_term: 
| term MULT fact                                                    { BinaryOp($1,$3,Mult) }
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
  { annotate ~loc:(Location($startpos, $endpos)) x}
;
%%