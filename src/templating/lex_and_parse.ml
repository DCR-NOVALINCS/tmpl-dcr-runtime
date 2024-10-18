open Syntax

let parse_with_error_handling lexbuf (parse_fun : Lexing.lexbuf -> 'a) =
  try Ok (parse_fun lexbuf) with
  | Lexer.Error message ->
      Error {
        location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p);
        message = "Lexing error: " ^ message;
        filepath = lexbuf.lex_curr_p.pos_fname
      }
  | Parser.Error ->
      Error {
        location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p);
        message = "Syntax error";
        filepath = lexbuf.lex_curr_p.pos_fname
      }
  | End_of_file ->
      Error {
        location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p);
        message = "Unexpected end of file";
        filepath = lexbuf.lex_curr_p.pos_fname
      }
  | _ -> Error { 
        location = Location (lexbuf.lex_start_p, lexbuf.lex_curr_p);
        message = "Unknown error"; 
        filepath = lexbuf.lex_curr_p.pos_fname 
      }

let parse_program lexbuf = 
  parse_with_error_handling lexbuf (Parser.main Lexer.read_token)

let parse_expression lexbuf =
  parse_with_error_handling lexbuf (Parser.main_expr Lexer.read_token)