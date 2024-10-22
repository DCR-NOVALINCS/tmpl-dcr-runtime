open Syntax
open Lexing
open Misc.Printing


let property_not_found ?(errors = []) p e =
  Error ({
    location = e.loc
    ; message = "Property " ^ p.data ^ " not found in " ^ string_of_expr e
    ; filepath = ""
  } :: errors)

and is_not_type ?(errors = []) expected expr =
  Error ({
    location = expr.loc
    ; message = Printf.sprintf "Expected type %s, but got %s" expected (string_of_expr expr)
    ; filepath = ""
  } :: errors)

and invalid_expr ?(errors = []) ?(loc = Nowhere) () =
  Error ({
    location = loc
    ; message = "Invalid expression"
    ; filepath = ""
  } :: errors)

and id_not_found ?(errors = []) id =
  Error ({
    location = id.loc
    ; message = "Identifier " ^ id.data ^ " not found"
    ; filepath = ""
  } :: errors)

and tmpl_not_found ?(errors = []) id =
  Error ({
    location = id.loc
    ; message = "Template " ^ id.data ^ " not found"
    ; filepath = ""
  } :: errors)

and invalid_annotation_value ?(errors = []) value ty =
  Error ({
    location = value.loc
    ; message = Printf.sprintf "Invalid annotation value %s for type %s" (string_of_expr value) (string_of_type_expr (annotate ty))
    ; filepath = ""
  } :: errors)

and lexing_error ?(errors = []) ?(filepath = "") lexbuf message =
  Error ({
    location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p);
    message = "Lexing error: " ^ message;
    filepath
  } :: errors)

and syntax_error ?(errors = []) ?(filepath = "") lexbuf =
  Error ({
    location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p);
    message = "Syntax error";
    filepath
  } :: errors)

and unexpected_eof ?(errors = []) ?(filepath = "") lexbuf =
  Error ({
    location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p);
    message = "Unexpected end of file";
    filepath
  } :: errors)

and unknown_error ?(errors = []) ?(filepath = "") lexbuf =
  Error ({
    location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p);
    message = "Something went wrong...";
    filepath
  } :: errors)

and event_not_found ?(errors = []) ?(loc=Nowhere) id =
  Error ({
    location = loc
    ; message = Printf.sprintf "Event %s not found" id
    ; filepath = ""
  } :: errors)

and event_not_enabled ?(errors = []) event =
  let (id, _) = event.data.info in
  Error ({
    location = event.loc
    ; message = Printf.sprintf "Event %s is not enabled" id.data
    ; filepath = ""
  } :: errors)

and invalid_guard_value ?(errors = []) value =
  Error ({
    location = value.loc
    ; message = Printf.sprintf "Invalid guard value. Expecting boolean expression, got %s" (string_of_expr value)
    ; filepath = ""
  } :: errors)

and invalid_command ?(errors = []) cmd =
  Error ({
    location = Nowhere
    ; message = Printf.sprintf "Invalid command %s" (String.concat " " cmd |> CString.colorize ~color:Yellow)
    ; filepath = ""
  } :: errors)

let get_line_content filepath line =
  if Sys.file_exists filepath then
    let file = open_in filepath in
    let rec read_line n =
      match input_line file with
      | content when n = line -> content
      | _ -> read_line (n + 1)
    in
    let line_content = read_line 1 in
    close_in file;
    line_content
  else ""

(*
=============================================================================
  Error printing
=============================================================================
*)

let extract_location_info loc =
  match loc with
  | Nowhere -> (0, 0, 0)
  | Location (start_pos, end_pos) ->
    let line = start_pos.Lexing.pos_lnum in
    let start_char = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
    let end_char = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol in
    (line, start_char, end_char)

(*┌*)
let print_error detailed_error =
  let { location ; message ; filepath } = detailed_error in
  let message_header =
    CPrinter.eprint "error: ";
    CPrinter.cprintln message;
    (* begin match location with
    | Nowhere -> ()
    | _ ->
      CPrinter.cprint " at ";
      CPrinter.cprintln ~color:Cyan (string_of_loc location)
    end *)
  in

  let message_file_section =
    let line, start_char, end_char = extract_location_info location in
    let line_size = String.length (string_of_int line) in
    let line_margin = String.make line_size ' ' in
    let marker =
      String.concat ""
        [ String.make start_char ' '
        ; CString.colorize ~color:Red
            (String.make (end_char - start_char) '^') ] in
    begin match filepath with
    | "" -> ()
      (* let line_content = "" in *)
      (* CPrinter.cprintf " stdin:%d:%d\n" line start_char ; *)
      (* CPrinter.cprintf " %s│\n" line_margin ;
      CPrinter.cprintf "%d │ %s\n" line line_content ;
      CPrinter.cprintf " %s│ %s" line_margin marker *)
    | _ ->
      let line_content = get_line_content filepath line in
      CPrinter.cprintf " %s:%d:%d\n" filepath line start_char ;
      CPrinter.cprintf " %s│\n" line_margin ;
      CPrinter.cprintf "%d │ %s\n" line line_content ;
      CPrinter.cprintf " %s│ %s" line_margin marker
      (* CPrinter.cprintf "  ──▶ %s:%d:%d\n" filepath line start_char ;
      CPrinter.cprintln "  │" ;
      CPrinter.cprintf "%d │ %s\n" line line_content ;
      CPrinter.cprintf "  │ %s" marker *)
    end in

  message_header;
  message_file_section
  (* CPrinter.cprintln "" *)
