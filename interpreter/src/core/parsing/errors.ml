open Ast
open Syntax
open Lexing
open Error
open Common
open Monads.ResultMonad

let lexing_error ?(errors = []) lexbuf message =
  fail
    ( { location=
          Location
            ( lexbuf.lex_start_p
            , lexbuf.lex_curr_p
            , Some lexbuf.lex_curr_p.pos_fname )
      ; message= "lexing error, " ^ message
      ; hint=
          Some
            "Check the syntax near the location. Ensure all tokens are valid."
      }
    :: errors )

let syntax_error ?(errors = []) lexbuf =
  fail
    ( { location=
          Location
            ( lexbuf.lex_start_p
            , lexbuf.lex_curr_p
            , Some lexbuf.lex_curr_p.pos_fname )
      ; message= "Syntax error"
      ; hint=
          Some
            "Check the syntax near the location. Ensure all constructs are correctly formed."
      }
    :: errors )

let unexpected_eof ?(errors = []) lexbuf =
  fail
    ( { location=
          Location
            ( lexbuf.lex_start_p
            , lexbuf.lex_curr_p
            , Some lexbuf.lex_curr_p.pos_fname )
      ; message= "Unexpected end of file"
      ; hint=
          Some
            "Ensure the file is complete and all constructs are properly closed."
      }
    :: errors )

and unknown_error ?(errors = []) lexbuf =
  fail
    ( { location=
          Location
            ( lexbuf.lex_start_p
            , lexbuf.lex_curr_p
            , Some lexbuf.lex_curr_p.pos_fname )
      ; message= "Something went wrong..."
      ; hint=
          Some "An unknown error occurred. Report this issue in the repository."
      }
    :: errors )
