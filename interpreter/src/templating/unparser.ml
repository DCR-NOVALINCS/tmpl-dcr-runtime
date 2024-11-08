open Syntax
open Misc.Printing
module String = CString

module UnparserList = struct
  let unparse_list ?(initial = "") ?(separator = "")
      ?(buffer = Buffer.create 100) unparse_fn list =
    match list with
    | [] -> ()
    | _ ->
        Buffer.add_string buffer @@ initial ;
        let rec unparse_list_aux = function
          | [] -> ()
          | [x] -> unparse_fn ~buffer x
          | x :: xs ->
              unparse_fn ~buffer x ;
              Buffer.add_string buffer @@ separator ;
              unparse_list_aux xs
        in
        unparse_list_aux list
end

module PlainUnparser = struct
  open UnparserList

  let rec unparse ?(indent = "") ?(abbreviated = true) ?(separator = "\n")
      ?((* ?(_colorize = false) *)
        should_print_template_decls = true) ?(should_print_events = true)
      ?(should_print_value = false)
      ?(should_print_executed_marking = false)
      ?(should_print_template_insts = true)
      ?(should_print_relations = true) ?(buffer = Buffer.create 100)
      program =
    let open Misc.Monads.FilterMonad in
    let print_template_decls =
      should_print_template_decls && List.length program.template_decls > 0
    in
    let print_events =
      should_print_events && List.length program.events > 0
    in
    let print_template_insts =
      should_print_template_insts && List.length program.template_insts > 0
    in
    let print_relations =
      should_print_relations && List.length program.relations > 0
    in
    return ()
    >>= ( print_template_decls
        , fun _ ->
            unparse_template_decls ~indent ~abbreviated
              ~separator:(separator ^ "\n") ~should_print_events
              ~should_print_template_insts ~should_print_relations ~buffer
              program.template_decls
            |> ignore ;
            Buffer.add_string buffer @@ separator ^ separator )
    >>= ( print_events || print_template_insts || print_relations
        , fun _ ->
            unparse_subprogram ~indent ~abbreviated
              ~separator:(separator ^ "\n") ~print_events
              ~print_value:(print_events && should_print_value)
              ~print_executed:
                (print_events && should_print_executed_marking)
              ~print_template_insts ~print_relations ~buffer
              (program.events, program.template_insts, program.relations)
            |> ignore ) ;
    Buffer.contents buffer

  and unparse_template_decls ?(indent = "") ?(abbreviated = true)
      ?(separator = "\n\n") ?(should_print_events = true)
      ?(should_print_template_insts = true)
      ?(should_print_relations = true) ?(buffer = Buffer.create 100)
      template_decls =
    let unparse_template_decl ?(indent = "") ?(abbreviated = true)
        ?(buffer = Buffer.create 100) template_decl =
      let {id; params; export_types; graph; export; _} = template_decl in
      (* let buffer = Buffer.create 100 in *)
      Buffer.add_string buffer @@ indent ;
      Buffer.add_string buffer @@ "tmpl " ;
      Buffer.add_string buffer @@ id.data ;
      Buffer.add_string buffer @@ "( " ;
      unparse_list ~buffer ~separator:", "
        (fun ~buffer (param, ty, _default) ->
          Buffer.add_string buffer @@ Printf.sprintf "%s: " param.data ;
          unparse_ty ~buffer ty |> ignore )
        params ;
      Buffer.add_string buffer @@ " )" ;
      (* Buffer.add_string buffer @@ Printf.sprintf "(%s)" (String.concat
         ", " params); *)
      (* List.iter (fun ty -> unparse_ty ~buffer ty) export_types; *)
      unparse_list ~buffer ~initial:": " ~separator:", "
        (fun ~buffer ty -> unparse_ty ~indent ~buffer ty |> ignore)
        export_types ;
      (* Buffer.add_string buffer @@ " {\n"; *)
      (* Buffer.add_string buffer @@ Printf.sprintf ": %s" (String.concat
         ", " export_types); *)
      Buffer.add_string buffer @@ " {\n" ;
      Buffer.add_string buffer @@ indent ;
      let graph_indent = indent ^ "  " in
      let tmpl_events, tmpl_insts, tmpl_relations = graph in
      unparse_subprogram ~indent:graph_indent ~abbreviated ~separator
        ~print_events:(should_print_events && List.length tmpl_events > 0)
        ~print_template_insts:
          (should_print_template_insts && List.length tmpl_insts > 0)
        ~print_relations:
          (should_print_relations && List.length tmpl_relations > 0)
        ~buffer graph
      |> ignore ;
      Buffer.add_string buffer @@ "\n" ^ indent ;
      Buffer.add_string buffer @@ indent ^ "}" ;
      (* let export = List.map (fun ex -> ex.data) export in
         Buffer.add_string buffer @@ String.concat ", " export *)
      unparse_list ~buffer ~initial:" => " ~separator:", "
        (fun ~buffer ex -> Buffer.add_string buffer @@ ex.data)
        export
    in
    unparse_list ~buffer ~separator
      (fun ~buffer template_decl ->
        unparse_template_decl ~indent ~abbreviated ~buffer template_decl )
      template_decls ;
    Buffer.contents buffer

  and unparse_subprogram ?(indent = "") ?(abbreviated = true)
      ?(separator = "\n") ?(print_events = true) ?(print_value = false)
      ?(print_executed = false) ?(print_template_insts = true)
      ?(print_relations = true) ?(buffer = Buffer.create 100)
      (events, insts, relations) =
    let open Misc.Monads.FilterMonad in
    return ()
    >>= ( print_events
        , fun _ ->
            unparse_events ~indent ~abbreviated ~print_value
              ~print_executed ~buffer events
            |> ignore )
    >>= ( print_template_insts
        , fun _ ->
            Buffer.add_string buffer @@ separator ;
            unparse_template_insts ~indent ~buffer insts |> ignore )
    >>= ( print_relations
        , fun _ ->
            Buffer.add_string buffer @@ separator ;
            unparse_relations ~indent ~abbreviated ~buffer relations
            |> ignore ) ;
    Buffer.contents buffer

  and unparse_events ?(indent = "") ?(abbreviated = true)
      ?(print_value = false) ?(print_executed = false)
      ?(buffer = Buffer.create 100) events =
    let unparse_info ?(indent = "") ?(buffer = Buffer.create 100)
        (id, label) =
      Buffer.add_string buffer @@ indent ;
      Buffer.add_string buffer
      @@ Printf.sprintf "(%s:%s)" id.data label.data ;
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
              unparse_ty ~buffer:inner_buffer ty |> ignore ) ;
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
        if marking.pending.data then
          Buffer.add_string buffer @@ pending_mark ;
        if not marking.included.data then
          Buffer.add_string buffer @@ excluded_mark ;
        ()
      in
      let unparse_marking_extended ?(indent = "")
          ?(buffer = Buffer.create 100) marking =
        Buffer.add_string buffer @@ indent ;
        Buffer.add_string buffer
        @@ Printf.sprintf "{ ex = %b; res = %b; in = %b }"
             marking.executed.data marking.pending.data
             marking.included.data ;
        ()
      in
      if abbreviated then
        unparse_marking_abbreviated ~indent ~buffer marking.data
      else unparse_marking_extended ~indent ~buffer marking.data
    in
    let unparse_event ?(indent = "") ?(abbreviated = true)
        ?(buffer = Buffer.create 100) event =
      let {info; io; marking; annotations} = event.data in
      Buffer.add_string buffer @@ indent ;
      unparse_marking ~abbreviated ~buffer marking ;
      unparse_info ~buffer info ;
      unparse_io ~buffer io ;
      if print_value then (
        Buffer.add_string buffer @@ " -> " ;
        unparse_expr ~buffer marking.data.value |> ignore ) ;
      (* Buffer.add_string buffer @@ " - "; *)
      unparse_annotations ~buffer annotations |> ignore ;
      ()
    in
    unparse_list ~buffer ~separator:"\n"
      (fun ~buffer event ->
        unparse_event ~indent ~abbreviated ~buffer event )
      events ;
    Buffer.contents buffer

  and unparse_template_insts ?(indent = "") ?(buffer = Buffer.create 100)
      template_insts =
    let unparse_inst ?(indent = "") ?(buffer = Buffer.create 100) inst =
      let {tmpl_id; args; x; tmpl_annotations} = inst in
      Buffer.add_string buffer @@ indent ;
      Buffer.add_string buffer @@ tmpl_id.data ;
      Buffer.add_string buffer @@ "(" ;
      unparse_list ~buffer ~separator:", "
        (fun ~buffer (arg_name, expr) ->
          Buffer.add_string buffer @@ arg_name.data ;
          Buffer.add_string buffer @@ " = " ;
          unparse_expr ~buffer expr |> ignore )
        args ;
      (* List.iter (fun (arg_name, expr) -> Buffer.add_string buffer @@
         Printf.sprintf "%s = " arg_name.data; unparse_expr ~buffer expr; )
         args; *)
      Buffer.add_string buffer @@ ")" ;
      (* Buffer.add_string buffer @@ " => "; *)
      unparse_list ~buffer ~initial:" => " ~separator:", "
        (fun ~buffer x -> Buffer.add_string buffer @@ x.data)
        x ;
      (* Buffer.add_string buffer @@ " - "; *)
      unparse_annotations ~indent ~buffer tmpl_annotations |> ignore ;
      ()
    in
    unparse_list ~buffer ~separator:"\n"
      (fun ~buffer inst -> unparse_inst ~indent ~buffer inst)
      template_insts ;
    Buffer.contents buffer

  and unparse_relations ?(indent = "") ?(abbreviated = true)
      ?(should_print_events = true) ?(should_print_template_insts = true)
      ?(buffer = Buffer.create 100) relations =
    let unparse_relation_arrow ~arrow_start
        ~guard:(guard_expr, guard_buffer) ?(buffer = Buffer.create 100)
        ~end_symbol _ =
      Buffer.add_string buffer @@ arrow_start ;
      ( match guard_expr.data with
      | True -> ()
      | _ ->
          Buffer.add_string buffer @@ "[" ;
          unparse_expr ~buffer:guard_buffer guard_expr |> ignore ;
          Buffer.add_buffer buffer guard_buffer ;
          Buffer.add_string buffer @@ "]" ) ;
      Buffer.add_string buffer @@ end_symbol
    in
    let unparse_relation_type ?(indent = "") ?(buffer = Buffer.create 100)
        ~guard relation_type =
      Buffer.add_string buffer @@ indent ;
      let arrow_start =
        match relation_type with Response -> "*-" | _ -> "-"
      in
      let arrow_end =
        match relation_type with
        | Condition -> "->*"
        | Include -> "->+"
        | Exclude -> "->%"
        | Milestone -> "-><>"
        | _ -> "->"
      in
      unparse_relation_arrow ~arrow_start ~guard ~end_symbol:arrow_end
        ~buffer ()
      |> ignore
    in
    let unparse_relation ?(indent = "") ?(abbreviated = true)
        ?(buffer = Buffer.create 100) relation =
      let guard_buffer = Buffer.create 100 in
      Buffer.add_string buffer @@ indent ;
      match relation.data with
      | ControlRelation (from, guard, dest, t, annot) ->
          Buffer.add_string buffer @@ from.data ;
          Buffer.add_string buffer @@ " " ;
          unparse_relation_type ~buffer ~guard:(guard, guard_buffer) t
          |> ignore ;
          Buffer.add_string buffer @@ " " ;
          Buffer.add_string buffer @@ dest.data ;
          unparse_annotations ~buffer annot |> ignore ;
          ()
      | SpawnRelation (from, guard, subprogram, annot) ->
          Buffer.add_string buffer @@ from.data ;
          Buffer.add_string buffer @@ " " ;
          unparse_relation_arrow ~arrow_start:"-"
            ~guard:(guard, guard_buffer) ~end_symbol:"->>" ~buffer ()
          |> ignore ;
          (* Buffer.add_string buffer @@ "-"; *)
          Buffer.add_string buffer @@ " {\n" ;
          let graph_indent = indent ^ "  " in
          let spawn_events, spawn_insts, spawn_relations = subprogram in
          unparse_subprogram ~indent:graph_indent ~separator:"\n"
            ~print_events:
              (should_print_events && List.length spawn_events > 0)
            ~print_template_insts:
              (should_print_template_insts && List.length spawn_insts > 0)
            ~print_relations:(List.length spawn_relations > 0)
            ~abbreviated ~buffer subprogram
          |> ignore ;
          Buffer.add_string buffer @@ "\n" ^ indent ^ "}" ;
          (* Buffer.add_string buffer @@ " - "; *)
          unparse_annotations ~indent ~buffer annot |> ignore
    in
    unparse_list ~buffer ~separator:"\n"
      (fun ~buffer relation ->
        unparse_relation ~indent ~abbreviated ~buffer relation )
      relations ;
    Buffer.contents buffer

  and unparse_annotations ?(indent = "") ?(buffer = Buffer.create 100)
      annotations =
    let unparse_annotation ?(indent = "") ?(buffer = Buffer.create 100)
        annotation =
      match annotation with
      | When expr ->
          let expr_buffer = Buffer.create 100 in
          Buffer.add_string buffer @@ "when " ;
          unparse_expr ~indent ~buffer:expr_buffer expr |> ignore ;
          Buffer.add_buffer buffer expr_buffer ;
          ()
      | Foreach (x, l) ->
          let l_buffer = Buffer.create 100 in
          Buffer.add_string buffer @@ "foreach " ;
          Buffer.add_string buffer @@ x.data ;
          Buffer.add_string buffer @@ " in " ;
          unparse_expr ~indent ~buffer:l_buffer l |> ignore ;
          Buffer.add_buffer buffer l_buffer ;
          ()
    in
    unparse_list ~buffer ~initial:" - " ~separator:" | "
      (fun ~buffer annotation ->
        unparse_annotation ~indent ~buffer annotation )
      annotations ;
    Buffer.contents buffer

  and unparse_ty ?(indent = "") ?(buffer = Buffer.create 100) ty =
    ( match ty.data with
    | UnitTy -> Buffer.add_string buffer @@ "()"
    | IntTy -> Buffer.add_string buffer @@ "Number"
    | BoolTy -> Buffer.add_string buffer @@ "Boolean"
    | StringTy -> Buffer.add_string buffer @@ "String"
    | EventTy s -> Buffer.add_string buffer @@ s
    | RecordTy fields ->
        Buffer.add_string buffer @@ "{ " ;
        unparse_list ~buffer ~separator:", "
          (fun ~buffer (field, ty) ->
            Buffer.add_string buffer @@ field.data ;
            Buffer.add_string buffer @@ ": " ;
            unparse_ty ~indent ~buffer ty |> ignore )
          fields ;
        (* List.iter (fun (field, ty) -> Buffer.add_string buffer @@
           field.data; Buffer.add_string buffer @@ ": "; unparse_ty ~indent
           ~buffer ty; ) fields; *)
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
    | True -> Buffer.add_string buffer @@ "true"
    | False -> Buffer.add_string buffer @@ "false"
    | IntLit i -> Buffer.add_string buffer @@ string_of_int i
    | StringLit s -> Buffer.add_string buffer @@ s
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
    | Trigger -> Buffer.add_string buffer @@ "@trigger"
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
    | Record fields ->
        Buffer.add_string buffer @@ "{ " ;
        unparse_list ~buffer ~separator:", "
          (fun ~buffer (field, e) ->
            Buffer.add_string buffer @@ field.data ;
            Buffer.add_string buffer @@ ": " ;
            unparse_expr ~indent ~buffer e |> ignore )
          fields ;
        Buffer.add_string buffer @@ " }"
    | _ -> Buffer.add_string buffer @@ "..." ) ;
    Buffer.contents buffer
end

(* module JsonUnparser: UnparserCommon = struct open Yojson.Safe

   let rec unparse ?(indent = "") ?(abbreviated = true) ?(separator = "\n")
   ?(buffer = Buffer.create 100) program = yojson_of_program program end *)
