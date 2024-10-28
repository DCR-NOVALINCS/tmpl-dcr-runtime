open Syntax
open Lexing
open Misc.Printing
open Ppx_yojson_conv_lib.Yojson_conv

type detailed_error = 
  { location : loc
  ; message : string
  ; hint : string option
  } [@@deriving yojson]

let property_not_found ?(errors = []) p e =
  Error ({
    location = e.loc
    ; message = "Property " ^ (CString.colorize ~color:Yellow p.data) ^ " not found in " ^ (CString.colorize ~color:Yellow @@ string_of_expr e)
    ; hint = Some "Ensure the property is declared and in scope. Check for typos."
  } :: errors)

and is_not_type ?(errors = []) expected expr =
  Error ({
    location = expr.loc
    ; message = Printf.sprintf "Expected type %s, but got %s" (CString.colorize ~color:Yellow expected) (CString.colorize ~color:Yellow @@ string_of_expr expr)
    ; hint = Some ("Verify the type of the expression " ^ ( CString.colorize ~color:Yellow @@ string_of_expr expr) ^ ". Check for type mismatches or any typos.")
  } :: errors)

and invalid_expr ?(errors = []) ?(loc = Nowhere) () =
  Error ({
    location = loc
    ; message = "Invalid expression"
    ; hint = Some "Check the syntax and structure of the expression. Ensure all definitions are correctly used."
  } :: errors)

and id_not_found ?(errors = []) id =
  Error ({
    location = id.loc
    ; message = "Identifier " ^ (CString.colorize ~color:Yellow id.data) ^ " not found"
    ; hint = Some "Ensure the identifier is declared and in scope. Check for typos."
  } :: errors)

and tmpl_not_found ?(errors = []) id =
  Error ({
    location = id.loc
    ; message = "Template " ^ (CString.colorize ~color:Yellow id.data) ^ " not found"
    ; hint = Some "Ensure the template is declared at the top of the file. Check for typos."
  } :: errors)

and invalid_annotation_value ?(errors = []) value ty =
  Error ({
    location = value.loc
    ; message = Printf.sprintf "Invalid annotation value %s for type %s" (CString.colorize ~color:Yellow @@ string_of_expr value) (CString.colorize ~color:Yellow @@ string_of_type_expr (annotate ty))
    ; hint = Some "Verify the annotation value matches the expected type. Check for type mismatches or any typos."
  } :: errors)

and lexing_error ?(errors = []) lexbuf message =
  Error ({
    location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p, Some lexbuf.lex_curr_p.pos_fname)
    ; message = "Lexing error: " ^ message
    ; hint = Some "Check the syntax near the error location. Ensure all tokens are valid."
  } :: errors)

and syntax_error ?(errors = []) lexbuf =
  Error ({
    location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p, Some lexbuf.lex_curr_p.pos_fname)
    ; message = "Syntax error"
    ; hint = Some "Check the syntax near the error location. Ensure all constructs are correctly formed."
  } :: errors)

and unexpected_eof ?(errors = []) lexbuf =
  Error ({
    location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p, Some lexbuf.lex_curr_p.pos_fname)
    ; message = "Unexpected end of file"
    ; hint = Some "Ensure the file is complete and all constructs are properly closed."
  } :: errors)

and unknown_error ?(errors = []) lexbuf =
  Error ({
    location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p, Some lexbuf.lex_curr_p.pos_fname)
    ; message = "Something went wrong..."
    ; hint = Some "An unknown error occurred. Report this issue in the repository."
  } :: errors)

and event_not_found ?(errors = []) ?(loc=Nowhere) id =
  Error ({
    location = loc
    ; message = Printf.sprintf "Event %s not found" (CString.colorize ~color:Yellow id)
    ; hint = Some "Ensure the event is declared and in scope. Check for typos."
  } :: errors)

and event_not_enabled ?(errors = []) event =
  let (id, _) = event.data.info in
  Error ({
    location = event.loc
    ; message = Printf.sprintf "Event %s is not enabled" (CString.colorize ~color:Yellow id.data)
    ; hint = Some "Check any relations or conditions that might be blocking this event."
  } :: errors)

and invalid_guard_value ?(errors = []) value =
  Error ({
    location = value.loc
    ; message = Printf.sprintf "Invalid guard value. Expecting boolean expression, got %s" (CString.colorize ~color:Yellow @@ string_of_expr value)
    ; hint = Some "Ensure the guard value is a boolean expression."
  } :: errors)

and invalid_command ?(errors = []) ?nearest ?(distance = -1) cmd =
  let guess = match nearest with
  | Some n when distance > 0 -> "Did you mean " ^ (CString.colorize ~color:Green n) ^ "?"
  | Some _ when distance = 0 -> "Did you miss some parameters?" 
  | _ -> "" in
  Error ({
    location = Nowhere
    ; message = Printf.sprintf "Invalid command %s" (String.concat " " cmd |> CString.colorize ~color:Yellow)
    ; hint = Some (guess ^ " Use " ^ CString.colorize ~color:Green "help" ^ " to see the available commands.")
  } :: errors)

(*
=============================================================================
  Error printing
=============================================================================
*)

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

let extract_location_info loc =
  match loc with
  | Nowhere -> (None, 0, 0, 0)
  | Location (start_pos, end_pos, filename) ->
    let line = start_pos.Lexing.pos_lnum in
    let start_char = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
    let end_char = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol in
    (filename, line, start_char, end_char)

(*┌*)
let print_error detailed_error =
  let { location ; message ; hint } = detailed_error in
  let message_header =
    CPrinter.eprint "error: ";
    CPrinter.cprintln message;
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
    | None -> ()
    | Some filepath ->
      let line_content = get_line_content filepath line in
      CPrinter.cprintf " %s:%d:%d\n" filepath line (start_char + 1) ;
      CPrinter.cprintf " %s│\n" line_margin ;
      CPrinter.cprintf "%d │ %s\n" line line_content ;
      CPrinter.cprintf " %s│ %s" line_margin marker;
      CPrinter.cprintln ""
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
