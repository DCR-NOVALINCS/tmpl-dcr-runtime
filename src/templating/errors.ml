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