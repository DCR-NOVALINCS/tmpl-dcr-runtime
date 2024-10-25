open Syntax
open Misc.Printing

module String = CString

let rec unparse ?(indent = "") ?(abbreviated = true) program =
  let buffer = Buffer.create 100 in
  unparse_template_decls ~indent ~abbreviated ~buffer program.template_decls;
  unparse_subprogram ~indent ~abbreviated ~buffer (program.events, program.template_insts, program.relations);
  Buffer.contents buffer
  

and unparse_list ?(separator = "") ~buffer unparse_fn = 
  function
  | [] -> ()
  | [x] -> unparse_fn ~buffer x
  | x :: xs -> 
    unparse_fn ~buffer x;
    Buffer.add_string buffer @@ separator;
    unparse_list ~separator ~buffer unparse_fn xs;
    ()

and unparse_template_decls ?(indent = "") ?(abbreviated = true) ~buffer =
  let unparse_template_decl ?(indent = "") ?(abbreviated = true) ~buffer template_decl = 
    let { id; params; export_types; graph; export; _ } = template_decl in
    (* let buffer = Buffer.create 100 in *)
    Buffer.add_string buffer @@ indent;
    Buffer.add_string buffer @@ "tmpl ";
    Buffer.add_string buffer @@ id.data;
    Buffer.add_string buffer @@ "(";
    unparse_list ~buffer ~separator:", " (fun ~buffer (param, ty, _default) -> 
      Buffer.add_string buffer @@ Printf.sprintf " %s: " param.data;
      unparse_ty ~buffer ty;
    ) params;
    Buffer.add_string buffer @@ ")";
    (* Buffer.add_string buffer @@ Printf.sprintf "(%s)" (String.concat ", " params); *)
    List.iter (fun ty -> unparse_ty ~buffer ty) export_types;
    (* unparse_list ~buffer ~separator:", " (fun ~buffer ty -> unparse_ty ~indent ~buffer ty |> ignore) export_types; *)
    (* Buffer.add_string buffer @@ " {\n"; *)
    (* Buffer.add_string buffer @@ Printf.sprintf ": %s" (String.concat ", " export_types); *)
    Buffer.add_string buffer @@ " {\n";
    Buffer.add_string buffer @@ indent;
    let graph_indent = indent ^ "  " in
    unparse_subprogram ~indent:graph_indent ~abbreviated ~buffer graph;
    Buffer.add_string buffer @@ "\n" ^ indent;
    Buffer.add_string buffer @@ "} => ";
    let export = List.map (fun ex -> ex.data) export in
    Buffer.add_string buffer @@ String.concat ", " export
  in
  function
  | [] -> ()
  | [template_decl] -> unparse_template_decl ~indent ~abbreviated ~buffer template_decl
  | template_decl :: template_decls -> 
    unparse_template_decl ~indent ~abbreviated ~buffer template_decl;
    Buffer.add_string buffer @@ "\n";
    unparse_template_decls ~indent ~abbreviated ~buffer template_decls

and unparse_subprogram ?(indent = "") ?(abbreviated = true) ~buffer (events, insts, relations) = 
  unparse_events ~indent ~abbreviated ~buffer events;
  unparse_template_insts ~indent ~buffer insts;
  unparse_relations ~indent ~abbreviated ~buffer relations

and unparse_events ?(indent = "") ?(abbreviated = true) ~buffer = 
  let unparse_info ?(indent = "") ~buffer (id, label) = 
    Buffer.add_string buffer @@ indent;
    Buffer.add_string buffer @@ Printf.sprintf "(%s:%s)" id.data label.data;
    ()
  in
  let unparse_io ?(indent = "") ~buffer io = 
    Buffer.add_string buffer @@ indent;
    let inner_buffer = Buffer.create 100 in 
    match io.data with
    | Input ty -> 
        unparse_ty ~buffer:inner_buffer ty;
        Buffer.add_string buffer @@ "[?";
        Buffer.add_buffer buffer inner_buffer;
        Buffer.add_string buffer @@ "]"
    | Output expr -> 
        unparse_expr ~buffer:inner_buffer expr;
        Buffer.add_string buffer @@ "[";
        Buffer.add_buffer buffer inner_buffer;
        Buffer.add_string buffer @@ "]"
  in
  let unparse_marking ?(indent = "") ?(abbreviated = true) ~buffer marking = 
    let unparse_marking_abbreviated 
        ?(indent = "") 
        ?(excluded_mark = "%") 
        ?(pending_mark = "!") 
        ?(executed_mark = "✓") 
        ~buffer marking = 
      Buffer.add_string buffer @@ indent;
      if not marking.included.data then 
        Buffer.add_string buffer @@ excluded_mark;
      if not marking.pending.data then
        Buffer.add_string buffer @@ pending_mark;
      if marking.executed.data then
        Buffer.add_string buffer @@ executed_mark;
      ()
      in
      let unparse_marking_extended ?(indent = "") ~buffer marking = 
        Buffer.add_string buffer @@ indent;
        Buffer.add_string buffer @@ Printf.sprintf "{ ex = %b; res = %b; in = %b }" marking.executed.data marking.pending.data marking.included.data;
        ()
      in
      if abbreviated then 
        unparse_marking_abbreviated ~indent ~buffer marking.data
      else 
        unparse_marking_extended ~indent ~buffer marking.data
    in
  let unparse_event ?(indent = "") ?(abbreviated = true) ~buffer event = 
    let { info; io; marking; annotations } = event.data in
    Buffer.add_string buffer @@ indent;
    unparse_info ~indent ~buffer info;
    unparse_io ~indent ~buffer io;
    unparse_marking ~indent ~abbreviated ~buffer marking;
    Buffer.add_string buffer @@ " - ";
    unparse_annotations ~indent ~buffer annotations;
    ()
  in
  function
  | [] -> ()
  | [event] -> unparse_event ~indent ~abbreviated ~buffer event
  | event :: events -> 
    unparse_event ~indent ~abbreviated ~buffer event;
    Buffer.add_string buffer @@ "\n";
    unparse_events ~indent ~abbreviated ~buffer events    

and unparse_template_insts ?(indent = "") ~buffer = 
  let unparse_inst ?(indent = "") ~buffer inst = 
    let { tmpl_id; args; x; tmpl_annotations } = inst in
    Buffer.add_string buffer @@ indent;
    Buffer.add_string buffer @@ tmpl_id.data;
    Buffer.add_string buffer @@ "(";
    List.iter (fun (arg_name, expr) -> 
      Buffer.add_string buffer @@ Printf.sprintf " %s: " arg_name.data;
      unparse_expr ~buffer expr;
      ) args;
    Buffer.add_string buffer @@ ")";
    Buffer.add_string buffer @@ " => ";
    List.iter (fun x -> Buffer.add_string buffer @@ x.data) x;
    Buffer.add_string buffer @@ " - ";
    unparse_annotations ~indent ~buffer tmpl_annotations;
    ()
  in
  function
  | [] -> ()
  | [inst] -> unparse_inst ~indent ~buffer inst
  | inst :: insts -> 
    unparse_inst ~indent ~buffer inst;
    Buffer.add_string buffer @@ "\n";
    unparse_template_insts ~indent ~buffer insts

and unparse_relations ?(indent = "") ?(abbreviated = true) ~buffer = 
  let unparse_relation_type ?(indent = "") ~buffer ~inner_buffer relation_type = 
    Buffer.add_string buffer @@ indent;
    let arrow_start = match relation_type with
    | Response -> "*-"
    | _ -> "-" in
    let arrow_end = match relation_type with
    | Condition -> "->*"
    | Include -> "->+"
    | Exclude -> "->%%"
    | Milestone -> "-><>"
    | _ -> "->" in
    Buffer.add_string buffer @@ arrow_start;
    Buffer.add_string buffer @@ "[";
    Buffer.add_buffer buffer inner_buffer;
    Buffer.add_string buffer @@ "]"; 
    Buffer.add_string buffer @@ arrow_end in
  let unparse_relation ?(indent = "") ?(abbreviated = true) ~buffer relation =
    let guard_buffer = Buffer.create 100 in
    Buffer.add_string buffer @@ indent;
    match relation.data with
    | ControlRelation (from, guard, dest, t, annot) ->
      Buffer.add_string buffer @@ from.data;
      unparse_expr ~indent ~buffer:guard_buffer guard;
      unparse_relation_type ~indent ~buffer ~inner_buffer:guard_buffer t;
      Buffer.add_string buffer @@ dest.data;
      unparse_annotations ~indent ~buffer annot;
      ()
    | SpawnRelation (from, guard, subprogram, annot) ->
      Buffer.add_string buffer @@ from.data;
      unparse_expr ~indent ~buffer:guard_buffer guard;
      Buffer.add_string buffer @@ "-";
      unparse_subprogram ~indent:(indent ^ "  ") ~abbreviated ~buffer subprogram;
      Buffer.add_string buffer @@ " - ";
      unparse_annotations ~indent ~buffer annot;
      
    in
    function
    | [] -> ()
    | [relation] -> unparse_relation ~indent ~abbreviated ~buffer relation
    | relation :: relations -> 
      unparse_relation ~indent ~abbreviated ~buffer relation;
      Buffer.add_string buffer @@ "\n";
      unparse_relations ~indent ~abbreviated ~buffer relations

and unparse_annotations ?(indent = "") ~buffer = 
  let unparse_annotation ?(indent = "") ~buffer annotation = 
    match annotation with
    | When expr -> 
      let expr_buffer = Buffer.create 100 in
      Buffer.add_string buffer @@ "when ";
      unparse_expr ~indent ~buffer:expr_buffer expr;
      Buffer.add_string buffer @@ "(";
      Buffer.add_buffer buffer expr_buffer;
      Buffer.add_string buffer @@ ")";
      ()
    | Foreach (x, l) -> 
      let l_buffer = Buffer.create 100 in
      Buffer.add_string buffer @@ "foreach ";
      Buffer.add_string buffer @@ x.data;
      Buffer.add_string buffer @@ " in ";
      unparse_expr ~indent ~buffer:l_buffer l;
      Buffer.add_string buffer @@ "(";
    in 
  function
  | [] -> ()
  | [annotation] -> unparse_annotation ~indent ~buffer annotation
  | annotation :: annotations -> 
    unparse_annotation ~indent ~buffer annotation;
    Buffer.add_string buffer @@ ", ";
    unparse_annotations ~indent ~buffer annotations

and unparse_ty ?(indent = "") ~buffer ty = 
  begin match ty.data with
  | UnitTy -> Buffer.add_string buffer @@ "()"
  | IntTy -> Buffer.add_string buffer @@ "Number"
  | BoolTy -> Buffer.add_string buffer @@ "Boolean"
  | StringTy -> Buffer.add_string buffer @@ "String"
  | EventTy s -> Buffer.add_string buffer @@ s
  | RecordTy fields -> 
    Buffer.add_string buffer @@ "{";
    List.iter (fun (field, ty) -> 
      Buffer.add_string buffer @@ field.data;
      Buffer.add_string buffer @@ ": ";
      unparse_ty ~indent ~buffer ty;
      ) fields;
    Buffer.add_string buffer @@ "}"
  | ListTy ty ->
    Buffer.add_string buffer @@ "[";
    unparse_ty ~indent ~buffer ty;
    Buffer.add_string buffer @@ "]"
  end

and unparse_expr ?(indent = "") ~buffer expr = 
  let unparse_binary_op ~buffer op = 
    match op with 
    | Add -> Buffer.add_string buffer @@ "+"
    | Mult -> Buffer.add_string buffer @@ "*"
    | Eq -> Buffer.add_string buffer @@ "=="
    | NotEq -> Buffer.add_string buffer @@ "!="
    | GreaterThan -> Buffer.add_string buffer @@ ">"
    | LessThan -> Buffer.add_string buffer @@ "<"
    | GreaterOrEqual -> Buffer.add_string buffer @@ ">="
    | LessOrEqual -> Buffer.add_string buffer @@ "<="
    | And -> Buffer.add_string buffer @@ "&&"
    | Or -> Buffer.add_string buffer @@ "||"
  in
  let unparse_unary_op ~buffer op = 
    match op with 
    | Negation -> Buffer.add_string buffer @@ "!"
    | Minus -> Buffer.add_string buffer @@ "-"
  in
  begin match expr.data with 
  | Unit -> Buffer.add_string buffer @@ "()"
  | True -> Buffer.add_string buffer @@ "true"
  | False -> Buffer.add_string buffer @@ "false"
  | IntLit i -> Buffer.add_string buffer @@ (string_of_int i)
  | StringLit s -> Buffer.add_string buffer @@ s
  | Parenthesized e -> 
    Buffer.add_string buffer @@ "(";
    unparse_expr ~indent ~buffer e;
    Buffer.add_string buffer @@ ")"
  | BinaryOp (e1, e2, op) -> 
    unparse_expr ~indent ~buffer e1;
    unparse_binary_op ~buffer op;
    unparse_expr ~indent ~buffer e2
  | UnaryOp (e, op) -> 
    unparse_unary_op ~buffer op;
    unparse_expr ~indent ~buffer e |> ignore
  | Identifier id -> Buffer.add_string buffer @@ id.data
  | Trigger -> Buffer.add_string buffer @@ "@trigger"
  | PropDeref (e, prop) -> 
    unparse_expr ~indent ~buffer e |> ignore;
    Buffer.add_string buffer @@ ".";
    Buffer.add_string buffer @@ prop.data
  | List es -> 
    Buffer.add_string buffer @@ "[";
    List.iter (fun e -> 
      unparse_expr ~indent ~buffer e;
      ) es;
    Buffer.add_string buffer @@ "]"
  | Record fields ->
    Buffer.add_string buffer @@ "{";
    List.iter (fun (field, e) -> 
      Buffer.add_string buffer @@ field.data;
      Buffer.add_string buffer @@ ": ";
      unparse_expr ~indent ~buffer e |> ignore;
      ) fields;
    Buffer.add_string buffer @@ "}"
  | _ -> Buffer.add_string buffer @@ "..."
  end