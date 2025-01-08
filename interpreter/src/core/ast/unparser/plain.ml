open Helper
open Common.Monads.FilterMonad
open Ast
open Syntax

let rec unparse ?(indent = "") ?(abbreviated = true) ?(separator = "\n")
    ?(should_print_template_decls = true) ?(should_print_events = true)
    ?(should_print_value = false) ?(should_print_executed_marking = false)
    ?(should_print_template_insts = true) ?(should_print_relations = true)
    ?(buffer = Buffer.create 100) program =
  let print_template_decls =
    should_print_template_decls && List.length program.template_decls > 0
  and print_events = should_print_events && List.length program.events > 0
  and print_template_insts =
    should_print_template_insts && List.length program.template_insts > 0
  and print_relations =
    should_print_relations && List.length program.relations > 0
  in
  return ()
  >>= ( print_template_decls
      , fun _ ->
          unparse_template_decls ~indent ~abbreviated ~separator
            ~should_print_events ~should_print_template_insts
            ~should_print_relations ~buffer program.template_decls
          |> ignore ;
          Buffer.add_string buffer @@ separator )
  >>= ( print_events || print_template_insts || print_relations
      , fun _ ->
          unparse_subprogram ~indent ~abbreviated ~separator ~print_events
            ~print_value:(print_events && should_print_value)
            ~print_executed:(print_events && should_print_executed_marking)
            ~print_template_insts ~print_relations ~buffer
            (program.events, program.template_insts, program.relations, [])
          |> ignore ) ;
  Buffer.contents buffer

and unparse_template_decls ?(indent = "") ?(abbreviated = true)
    ?(separator = "\n\n") ?(should_print_events = true)
    ?(should_print_template_insts = true) ?(should_print_relations = true)
    ?(buffer = Buffer.create 100) template_decls =
  unparse_list ~buffer ~separator
    (fun ~buffer template_decl ->
      unparse_template_decl ~indent ~abbreviated ~separator ~should_print_events
        ~should_print_template_insts ~should_print_relations ~buffer
        template_decl
      |> ignore ;
      Buffer.add_string buffer @@ separator )
    template_decls ;
  Buffer.contents buffer

and unparse_template_decl ?(indent = "") ?(abbreviated = true)
    ?(separator = "\n\n") ?(should_print_events = true)
    ?(should_print_template_insts = true) ?(should_print_relations = true)
    ?(buffer = Buffer.create 100) template_decl =
  let unparse_template_param ?(indent = "") ?(separator = ": ")
      ?(buffer = Buffer.create 100) (id, ty, default) =
    Buffer.add_string buffer @@ indent ;
    Buffer.add_string buffer @@ id.data ;
    Buffer.add_string buffer @@ separator ;
    unparse_ty ~buffer ~indent ty.data |> ignore ;
    match default with
    | None -> ()
    | Some value ->
        Buffer.add_string buffer @@ " = " ;
        unparse_expr ~buffer ~indent value |> ignore
  in
  let {id; params; export_types; graph; export; _} = template_decl in
  (* let buffer = Buffer.create 100 in *)
  Buffer.add_string buffer @@ indent ;
  Buffer.add_string buffer @@ "tmpl " ;
  Buffer.add_string buffer @@ id.data ;
  Buffer.add_string buffer @@ "( " ;
  unparse_list ~buffer ~separator:", "
    (fun ~buffer param -> unparse_template_param ~buffer param)
    params ;
  Buffer.add_string buffer @@ " )" ;
  unparse_list ~buffer ~initial:": " ~separator:", "
    (fun ~buffer label -> Buffer.add_string buffer @@ label.data)
    export_types ;
  Buffer.add_string buffer @@ " {\n" ;
  Buffer.add_string buffer @@ indent ;
  let graph_indent = indent ^ "  " in
  let tmpl_events, tmpl_insts, tmpl_relations, _tmpl_annotations = graph in
  unparse_subprogram ~indent:graph_indent ~abbreviated ~separator
    ~print_events:(should_print_events && List.length tmpl_events > 0)
    ~print_template_insts:
      (should_print_template_insts && List.length tmpl_insts > 0)
    ~print_relations:(should_print_relations && List.length tmpl_relations > 0)
    ~buffer graph
  |> ignore ;
  Buffer.add_string buffer @@ "\n" ^ indent ;
  Buffer.add_string buffer @@ indent ^ "}" ;
  (* let export = List.map (fun ex -> ex.data) export in Buffer.add_string
     buffer @@ String.concat ", " export *)
  unparse_list ~buffer ~initial:" => " ~separator:", "
    (fun ~buffer ex -> Buffer.add_string buffer @@ ex.data)
    export

and unparse_subprogram ?(indent = "") ?(abbreviated = true) ?(separator = "\n")
    ?(print_events = true) ?(print_value = false) ?(print_executed = false)
    ?(print_template_insts = true) ?(print_relations = true)
    ?(buffer = Buffer.create 100) (events, insts, relations, annotations) =
  return ()
  >>= ( print_events
      , fun _ ->
          unparse_events ~indent ~abbreviated ~print_value ~print_executed
            ~buffer events
          |> ignore )
  >>= ( print_events && print_template_insts
      , fun _ -> Buffer.add_string buffer @@ separator )
  >>= ( print_template_insts
      , fun _ -> unparse_template_insts ~indent ~buffer insts |> ignore )
  >>= ( (print_events || print_template_insts) && print_relations
      , fun _ -> Buffer.add_string buffer @@ separator )
  >>= ( print_relations
      , fun _ ->
          unparse_relations ~indent ~abbreviated ~buffer relations |> ignore )
  >>= ( true
      , fun _ ->
          Buffer.add_string buffer @@ separator ;
          unparse_annotations ~indent ~buffer annotations |> ignore ) ;
  Buffer.contents buffer

and unparse_events ?(indent = "") ?(abbreviated = true) ?(print_value = false)
    ?(print_executed = false) ?(buffer = Buffer.create 100) events =
  (* in *)
  unparse_list ~buffer ~separator:"\n"
    (fun ~buffer event ->
      ignore
        (unparse_event ~indent ~abbreviated ~print_value ~print_executed ~buffer
           event ) )
    events ;
  Buffer.contents buffer

and unparse_event ?(indent = "") ?(abbreviated = true) ?(print_value = false)
    ?(print_executed = false) ?(buffer = Buffer.create 100) event =
  let unparse_info ?(indent = "") ?(buffer = Buffer.create 100) (id, label) =
    Buffer.add_string buffer @@ indent ;
    Buffer.add_string buffer @@ Printf.sprintf "(%s:%s)" id.data label.data ;
    ()
  in
  let unparse_io ?(indent = "") ?(buffer = Buffer.create 100) io =
    Buffer.add_string buffer @@ indent ;
    let inner_buffer = Buffer.create 100 in
    match io.data with
    | Input ty ->
        ( match ty.data with
        | UnitTy -> ()
        | _ ->
            Buffer.add_string inner_buffer @@ ": " ;
            unparse_ty ~buffer:inner_buffer ty.data |> ignore ) ;
        Buffer.add_string buffer @@ "[?" ;
        Buffer.add_buffer buffer inner_buffer ;
        Buffer.add_string buffer @@ "]"
    | Output expr ->
        unparse_expr ~buffer:inner_buffer expr |> ignore ;
        Buffer.add_string buffer @@ "[" ;
        Buffer.add_buffer buffer inner_buffer ;
        Buffer.add_string buffer @@ "]"
  in
  let unparse_marking ?(indent = "") ?(abbreviated = true)
      ?(buffer = Buffer.create 100) marking =
    let unparse_marking_abbreviated ?(indent = "") ?(excluded_mark = "%")
        ?(pending_mark = "!") ?(executed_mark = "✓") ~buffer marking =
      Buffer.add_string buffer @@ indent ;
      if print_executed && marking.executed.data then
        Buffer.add_string buffer @@ executed_mark ;
      if marking.pending.data then Buffer.add_string buffer @@ pending_mark ;
      if not marking.included.data then
        Buffer.add_string buffer @@ excluded_mark ;
      ()
    in
    let unparse_marking_extended ?(indent = "") ?(buffer = Buffer.create 100)
        marking =
      Buffer.add_string buffer @@ indent ;
      Buffer.add_string buffer
      @@ Printf.sprintf "{ ex = %b; res = %b; in = %b }" marking.executed.data
           marking.pending.data marking.included.data ;
      ()
    in
    if abbreviated then unparse_marking_abbreviated ~indent ~buffer marking.data
    else unparse_marking_extended ~indent ~buffer marking.data
  in
  let {info; io; marking} = event.data in
  Buffer.add_string buffer @@ indent ;
  unparse_marking ~abbreviated ~buffer marking ;
  unparse_info ~buffer info ;
  unparse_io ~buffer io ;
  if print_value then (
    Buffer.add_string buffer @@ " -> " ;
    unparse_expr ~buffer !(marking.data.value) |> ignore ) ;
  Buffer.contents buffer

and unparse_template_insts ?(indent = "") ?(buffer = Buffer.create 100)
    template_insts =
  unparse_list ~buffer ~separator:"\n"
    (fun ~buffer inst ->
      ignore (unparse_template_inst ~indent ~buffer inst.data) )
    template_insts ;
  Buffer.contents buffer

and unparse_template_inst ?(indent = "") ?(buffer = Buffer.create 100) inst =
  let unparse_arg ?(indent = "") ?(separator = " = ")
      ?(buffer = Buffer.create 100) (arg_name, arg_expr) =
    Buffer.add_string buffer @@ indent ;
    Buffer.add_string buffer @@ arg_name.data ;
    Buffer.add_string buffer @@ separator ;
    unparse_expr ~buffer arg_expr |> ignore
  in
  let {tmpl_id; args; x} = inst in
  Buffer.add_string buffer @@ indent ;
  Buffer.add_string buffer @@ tmpl_id.data ;
  Buffer.add_string buffer @@ "(" ;
  unparse_list ~buffer ~separator:", "
    (fun ~buffer arg -> unparse_arg ~buffer ~separator:" = " arg)
    args ;
  Buffer.add_string buffer @@ ")" ;
  unparse_list ~buffer ~initial:" => " ~separator:", "
    (fun ~buffer x -> Buffer.add_string buffer @@ x.data)
    x ;
  Buffer.contents buffer

and unparse_relations ?(indent = "") ?(abbreviated = true)
    ?(should_print_events = true) ?(should_print_template_insts = true)
    ?(buffer = Buffer.create 100) relations =
  unparse_list ~buffer ~separator:"\n"
    (fun ~buffer relation ->
      ignore
        (unparse_relation ~indent ~abbreviated ~should_print_events
           ~should_print_template_insts ~buffer relation ) )
    relations ;
  Buffer.contents buffer

and unparse_relation ?(indent = "") ?(abbreviated = true)
    ?(should_print_events = true) ?(should_print_template_insts = true)
    ?(buffer = Buffer.create 100) relation =
  let unparse_relation_arrow ~arrow_start ~guard:(guard_expr, guard_buffer)
      ?(buffer = Buffer.create 100) ~end_symbol _ =
    Buffer.add_string buffer @@ arrow_start ;
    ( match guard_expr.data with
    | BoolLit true -> ()
    | _ ->
        Buffer.add_string buffer @@ "[" ;
        unparse_expr ~buffer:guard_buffer guard_expr |> ignore ;
        Buffer.add_buffer buffer guard_buffer ;
        Buffer.add_string buffer @@ "]" ) ;
    Buffer.add_string buffer @@ end_symbol
  in
  let unparse_relation_type ?(indent = "") ?(buffer = Buffer.create 100) ~guard
      relation_type =
    Buffer.add_string buffer @@ indent ;
    let arrow_start = match relation_type with Response -> "*-" | _ -> "-" in
    let arrow_end =
      match relation_type with
      | Condition -> "->*"
      | Include -> "->+"
      | Exclude -> "->%"
      | Milestone -> "-><>"
      | _ -> "->"
    in
    unparse_relation_arrow ~arrow_start ~guard ~end_symbol:arrow_end ~buffer ()
    |> ignore
  in
  let guard_buffer = Buffer.create 100 in
  Buffer.add_string buffer @@ indent ;
  ( match relation.data with
  | ControlRelation (from, guard, dest, t) ->
      Buffer.add_string buffer @@ from.data ;
      Buffer.add_string buffer @@ " " ;
      unparse_relation_type ~buffer ~guard:(guard, guard_buffer) t |> ignore ;
      Buffer.add_string buffer @@ " " ;
      Buffer.add_string buffer @@ dest.data
  | SpawnRelation (from, guard, subprogram) ->
      Buffer.add_string buffer @@ from.data ;
      Buffer.add_string buffer @@ " " ;
      unparse_relation_arrow ~arrow_start:"-" ~guard:(guard, guard_buffer)
        ~end_symbol:"->>" ~buffer ()
      |> ignore ;
      (* Buffer.add_string buffer @@ "-"; *)
      Buffer.add_string buffer @@ " {\n" ;
      let graph_indent = indent ^ "  " in
      let spawn_events, spawn_insts, spawn_relations, _spawn_annotations =
        subprogram
      in
      unparse_subprogram ~indent:graph_indent ~separator:"\n"
        ~print_events:(should_print_events && List.length spawn_events > 0)
        ~print_template_insts:
          (should_print_template_insts && List.length spawn_insts > 0)
        ~print_relations:(List.length spawn_relations > 0)
        ~abbreviated ~buffer subprogram
      |> ignore ;
      Buffer.add_string buffer @@ "\n" ^ indent ^ "}" ) ;
  Buffer.contents buffer

and unparse_annotations ?(indent = "") ?(abbreviated = true)
    ?(should_print_events = true) ?(should_print_template_insts = true)
    ?(buffer = Buffer.create 100) annotations =
  unparse_list ~buffer ~separator:"\n"
    (fun ~buffer annotation ->
      ignore
        (unparse_annotation ~indent ~abbreviated ~should_print_events
           ~should_print_template_insts ~buffer annotation ) )
    annotations ;
  Buffer.contents buffer

and unparse_annotation ?(indent = "") ?(abbreviated = true)
    ?(should_print_events = true) ?(should_print_template_insts = true)
    ?(buffer = Buffer.create 100) annotation =
  Buffer.add_string buffer @@ indent ;
  ( match annotation with
  | IfElse {condition; then_branch; else_branch} ->
      Buffer.add_string buffer @@ "if " ;
      unparse_expr ~buffer condition |> ignore ;
      Buffer.add_string buffer @@ ":\n" ;
      unparse_subprogram ~indent:(indent ^ "  ") ~buffer
        ~print_events:should_print_events ~abbreviated
        ~print_template_insts:should_print_template_insts then_branch
      |> ignore ;
      Buffer.add_string buffer @@ "\n" ;
      Buffer.add_string buffer @@ indent ;
      ( match else_branch with
      | None -> ()
      | Some branch ->
          Buffer.add_string buffer @@ "else:\n" ;
          unparse_subprogram ~indent:(indent ^ "  ") ~buffer branch |> ignore ) ;
      Buffer.add_string buffer @@ indent ;
      Buffer.add_string buffer @@ "/if\n" ;
      Buffer.add_string buffer @@ "\n"
  | Foreach (id, expr, body) ->
      Buffer.add_string buffer @@ "foreach " ;
      Buffer.add_string buffer @@ id.data ;
      Buffer.add_string buffer @@ " in " ;
      unparse_expr ~buffer expr |> ignore ;
      Buffer.add_string buffer @@ ":\n" ;
      unparse_subprogram ~indent:(indent ^ "  ") ~buffer
        ~print_events:should_print_events ~abbreviated
        ~print_template_insts:should_print_template_insts body
      |> ignore ;
      Buffer.add_string buffer @@ "\n" ;
      Buffer.add_string buffer @@ indent ;
      Buffer.add_string buffer @@ "/foreach\n" ) ;
  Buffer.contents buffer

and unparse_ty ?(indent = "") ?(buffer = Buffer.create 100) ty =
  ( match ty with
  | UnitTy -> Buffer.add_string buffer @@ "Unit"
  | IntTy -> Buffer.add_string buffer @@ "Number"
  | BoolTy -> Buffer.add_string buffer @@ "Boolean"
  | StringTy -> Buffer.add_string buffer @@ "String"
  | EventTy s -> Buffer.add_string buffer @@ s.data
  | RecordTy fields ->
      Buffer.add_string buffer @@ "{ " ;
      unparse_list ~buffer ~separator:", "
        (fun ~buffer (field, ty) ->
          Buffer.add_string buffer @@ field.data ;
          Buffer.add_string buffer @@ ": " ;
          unparse_ty ~indent ~buffer ty.data |> ignore )
        fields ;
      Buffer.add_string buffer @@ " }"
  | ListTy ty ->
      Buffer.add_string buffer @@ "List[" ;
      unparse_ty ~indent ~buffer ty |> ignore ;
      Buffer.add_string buffer @@ "]" ) ;
  Buffer.contents buffer

and unparse_expr ?(indent = "") ?(buffer = Buffer.create 100) expr =
  let unparse_binary_op ?(buffer = Buffer.create 100) op =
    match op with
    | Add -> Buffer.add_string buffer @@ "+"
    | Sub -> Buffer.add_string buffer @@ "-"
    | Mult -> Buffer.add_string buffer @@ "*"
    | Div -> Buffer.add_string buffer @@ "/"
    | Eq -> Buffer.add_string buffer @@ "=="
    | NotEq -> Buffer.add_string buffer @@ "!="
    | GreaterThan -> Buffer.add_string buffer @@ ">"
    | LessThan -> Buffer.add_string buffer @@ "<"
    | GreaterOrEqual -> Buffer.add_string buffer @@ ">="
    | LessOrEqual -> Buffer.add_string buffer @@ "<="
    | And -> Buffer.add_string buffer @@ "&&"
    | Or -> Buffer.add_string buffer @@ "||"
  in
  let unparse_unary_op ?(buffer = Buffer.create 100) op =
    match op with
    | Negation -> Buffer.add_string buffer @@ "!"
    | Minus -> Buffer.add_string buffer @@ "-"
  in
  ( match expr.data with
  | Unit -> Buffer.add_string buffer @@ "()"
  | BoolLit b ->
      let b_str = string_of_bool b in
      Buffer.add_string buffer @@ b_str
  | IntLit i -> Buffer.add_string buffer @@ string_of_int i
  | StringLit s ->
      Buffer.add_string buffer @@ "\"" ;
      Buffer.add_string buffer @@ s ;
      Buffer.add_string buffer @@ "\""
  | Parenthesized e ->
      Buffer.add_string buffer @@ "(" ;
      unparse_expr ~indent ~buffer e |> ignore ;
      Buffer.add_string buffer @@ ")"
  | BinaryOp (e1, e2, op) ->
      unparse_expr ~indent ~buffer e1 |> ignore ;
      Buffer.add_string buffer @@ " " ;
      unparse_binary_op ~buffer op ;
      Buffer.add_string buffer @@ " " ;
      unparse_expr ~indent ~buffer e2 |> ignore
  | UnaryOp (e, op) ->
      unparse_unary_op ~buffer op ;
      unparse_expr ~indent ~buffer e |> ignore
  | Identifier id -> Buffer.add_string buffer @@ id.data
  | Trigger -> Buffer.add_string buffer @@ trigger_id
  | PropDeref (e, prop) ->
      unparse_expr ~indent ~buffer e |> ignore ;
      Buffer.add_string buffer @@ "." ;
      Buffer.add_string buffer @@ prop.data
  | List es ->
      Buffer.add_string buffer @@ "[ " ;
      unparse_list ~buffer ~separator:", "
        (fun ~buffer e -> unparse_expr ~indent ~buffer e |> ignore)
        es ;
      Buffer.add_string buffer @@ " ]"
  | Range (s, e) ->
      Buffer.add_string buffer @@ "@range(" ;
      unparse_expr ~indent ~buffer s |> ignore ;
      Buffer.add_string buffer @@ ", " ;
      unparse_expr ~indent ~buffer e |> ignore ;
      Buffer.add_string buffer @@ ")"
  | Record fields ->
      Buffer.add_string buffer @@ "{ " ;
      unparse_list ~buffer ~separator:", "
        (fun ~buffer (field, e) ->
          Buffer.add_string buffer @@ field.data ;
          Buffer.add_string buffer @@ ": " ;
          unparse_expr ~indent ~buffer e |> ignore )
        fields ;
      Buffer.add_string buffer @@ " }"
  | EventRef event_ref ->
      Buffer.add_string buffer "ref " ;
      unparse_events ~indent ~buffer [!event_ref] |> ignore
  | _ -> Buffer.add_string buffer @@ "..." ) ;
  Buffer.contents buffer

and unparse_pos ?(indent = "") ?(separator = ":") ?(buffer = Buffer.create 100)
    pos =
  let line = pos.Lexing.pos_lnum in
  let start_char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Buffer.add_string buffer indent ;
  (* Printf.sprintf "%d:%d" line start_char |> Buffer.add_string buffer *)
  Buffer.add_string buffer @@ string_of_int line ;
  Buffer.add_string buffer @@ separator ;
  Buffer.add_string buffer @@ string_of_int start_char ;
  Buffer.contents buffer

and unparse_loc ?(indent = "") ?(separator = ":") ?(buffer = Buffer.create 100)
    loc =
  Buffer.add_string buffer indent ;
  ( match loc with
  | Nowhere -> Buffer.add_string buffer "?"
  | Location (start_pos, end_pos, filename) ->
      let filename = Option.value filename ~default:"" in
      let start_pos_string = unparse_pos start_pos in
      let end_pos_string = unparse_pos end_pos in
      (* Printf.sprintf "%s:%s:%s" filename start_pos_string end_pos_string *)
      Buffer.add_string buffer start_pos_string ;
      Buffer.add_string buffer separator ;
      Buffer.add_string buffer end_pos_string ;
      Buffer.add_string buffer separator ;
      Buffer.add_string buffer filename ) ;
  Buffer.contents buffer
