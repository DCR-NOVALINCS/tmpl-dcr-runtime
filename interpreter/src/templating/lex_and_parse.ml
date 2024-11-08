open Errors

let parse_with_error_handling ?(filepath = "") lexbuf
    (parse_fun : Lexing.lexbuf -> 'a) =
  try
    lexbuf.Lexing.lex_curr_p <-
      {lexbuf.Lexing.lex_curr_p with pos_fname= filepath} ;
    Ok (parse_fun lexbuf)
  with
  | Lexer.Error message -> lexing_error lexbuf message
  | Parser.Error -> syntax_error lexbuf
  | End_of_file -> unexpected_eof lexbuf
  | _ -> unknown_error lexbuf

let parse_program ?(filename = "") lexbuf =
  parse_with_error_handling ~filepath:filename lexbuf
    (Parser.main Lexer.read_token)

let parse_expression ?(filename = "") lexbuf =
  parse_with_error_handling ~filepath:filename lexbuf
    (Parser.main_expr Lexer.read_token)
