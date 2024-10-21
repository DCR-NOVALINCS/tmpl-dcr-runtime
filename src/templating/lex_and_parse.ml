open Syntax

let parse_with_error_handling ?(filepath = "") lexbuf (parse_fun : Lexing.lexbuf -> 'a) =
  try
    lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with pos_fname = filepath }; 
    Ok (parse_fun lexbuf) 
  with
  | Lexer.Error message ->
      Error {
        location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p);
        message = "Lexing error: " ^ message;
        filepath
      }
  | Parser.Error ->
      Error {
        location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p);
        message = "Syntax error";
        filepath
      }
  | End_of_file ->
      Error {
        location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p);
        message = "Unexpected end of file";
        filepath
      }
  | _ -> Error { 
        location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p);
        message = "Something went wrong..."; 
        filepath 
      }

let parse_program ?(filename = "") lexbuf = 
  parse_with_error_handling ~filepath:filename lexbuf (Parser.main Lexer.read_token)

let parse_expression ?(filename = "") lexbuf =
  parse_with_error_handling ~filepath:filename lexbuf (Parser.main_expr Lexer.read_token)