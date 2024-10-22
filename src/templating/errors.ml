open Syntax
open Lexing
open Misc.Printing

type detailed_error = 
  { location : loc
  ; message : string
  ; hint : string option
  }

let property_not_found ?(errors = []) p e =
  Error ({
    location = e.loc
    ; message = "Property " ^ p.data ^ " not found in " ^ string_of_expr e
    ; hint = Some "Maybe you forgot to declare it or it is not in scope?"
  } :: errors)

and is_not_type ?(errors = []) expected expr =
  Error ({
    location = expr.loc
    ; message = Printf.sprintf "Expected type %s, but got %s" expected (string_of_expr expr)
    ; hint = None
  } :: errors)

and invalid_expr ?(errors = []) ?(loc = Nowhere) () =
  Error ({
    location = loc
    ; message = "Invalid expression"
    ; hint = None
  } :: errors)

and id_not_found ?(errors = []) id =
  Error ({
    location = id.loc
    ; message = "Identifier " ^ id.data ^ " not found"
    ; hint = Some "Maybe you forgot to declare it?"
  } :: errors)

and tmpl_not_found ?(errors = []) id =
  Error ({
    location = id.loc
    ; message = "Template " ^ id.data ^ " not found"
    ; hint = Some "Maybe you forgot to declare the template on the top of the file?"
  } :: errors)

and invalid_annotation_value ?(errors = []) value ty =
  Error ({
    location = value.loc
    ; message = Printf.sprintf "Invalid annotation value %s for type %s" (string_of_expr value) (string_of_type_expr (annotate ty))
    ; hint = None
  } :: errors)

and lexing_error ?(errors = []) ?filepath lexbuf message =
  Error ({
    location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p, filepath)
    ; message = "Lexing error: " ^ message
    ; hint = None
  } :: errors)

and syntax_error ?(errors = []) ?filepath lexbuf =
  Error ({
    location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p, filepath)
    ; message = "Syntax error"
    ; hint = None
  } :: errors)

and unexpected_eof ?(errors = []) ?filepath lexbuf =
  Error ({
    location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p, filepath)
    ; message = "Unexpected end of file"
    ; hint = None
  } :: errors)

and unknown_error ?(errors = []) ?filepath lexbuf =
  Error ({
    location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p, filepath)
    ; message = "Something went wrong..."
    ; hint = None
  } :: errors)

and event_not_found ?(errors = []) ?(loc=Nowhere) id =
  Error ({
    location = loc
    ; message = Printf.sprintf "Event %s not found" id
    ; hint = Some "Maybe you forgot to declare it or it is not in scope?"
  } :: errors)

and event_not_enabled ?(errors = []) event =
  let (id, _) = event.data.info in
  Error ({
    location = event.loc
    ; message = Printf.sprintf "Event %s is not enabled" id.data
    ; hint = Some "Check any relations that might be blocking this event"
  } :: errors)

and invalid_guard_value ?(errors = []) value =
  Error ({
    location = value.loc
    ; message = Printf.sprintf "Invalid guard value. Expecting boolean expression, got %s" (string_of_expr value)
    ; hint = None
  } :: errors)

and invalid_command ?(errors = []) cmd =
  Error ({
    location = Nowhere
    ; message = Printf.sprintf "Invalid command %s" (String.concat " " cmd |> CString.colorize ~color:Yellow)
    ; hint = Some ("Maybe you mistyped the command? Try " ^ CString.colorize ~color:Green "help" ^ " to see the available commands")
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
  | Nowhere -> ("", 0, 0, 0)
  | Location (start_pos, end_pos, filename) ->
    let line = start_pos.Lexing.pos_lnum in
    let start_char = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
    let end_char = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol in
    let filepath = Option.value ~default:"" filename in
    (filepath, line, start_char, end_char)

(*┌*)
let print_error detailed_error =
  let { location ; message ; hint } = detailed_error in
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
    let (filepath, line, start_char, end_char) = extract_location_info location in
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

  let message_hint = 
    begin match hint with
    | None -> ()
    | Some message -> 
      CPrinter.cprint ~color:Cyan "hint: ";
      CPrinter.cprintln message;
    end in

  message_header;
  message_file_section;
  message_hint
  (* CPrinter.cprintln "" *)
