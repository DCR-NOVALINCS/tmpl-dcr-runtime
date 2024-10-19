{
open Parser
open Syntax

exception UnknownToken of string
exception Error of string

let sprintf  = Printf.sprintf
let ksprintf = Printf.ksprintf

(* This functions dont belongs here! *)
let char_to_string (c:char) = String.make 1 c

let error lexbuf fmt = 
    ksprintf (fun msg -> 
        raise (Error ((string_of_pos lexbuf.Lexing.lex_curr_p)^" "^msg))) fmt
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+
let id = (alpha) (alpha|digit|'_')*

let whitespace = [' ' '\t']
let newline = "\r\n" | '\r' | '\n'


rule read_token = parse
	| whitespace+      	{ read_token lexbuf }
	| newline			{ Lexing.new_line lexbuf; read_token lexbuf }
	| "###"				{ read_comment_block lexbuf }
    | "#"               { read_line_comment lexbuf }
	(* | ";;"             	{ EOL } *)
	| "true"         	{ TRUE } 
	| "false"           { FALSE } 
  (* dcr (unguarded) relations*)
	| "-->+"            { INCLUDE }
	| "-->%"            { EXCLUDE }
	| "-->*"            { CONDITION }
	| "*-->"            { RESPONSE }
	| "--><>"           { MILESTONE }
	| "-->>"            { SPAWN }
  (* guarded dcr relations - left guard *)
	| "-["              { LGUARD }
	| "*-["             { LGUARD_RESPONSE }
  (* guarded dcr relation - right guard *)
	| "]->+"            { RGUARD_INCLUDE}
	| "]->"             { RGUARD_RESPONSE }
	| "]->%"            { RGUARD_EXCLUDE}
	| "]->*"            { RGUARD_CONDITION}
	| "]-><>"           { RGUARD_MILESTONE}
	| "]->>"            { RGUARD_SPAWN}
	| "@trigger"        { TRIGGER }  
	| '\''              { STR (read_string (Buffer.create 20) lexbuf) }
	| '+' 				{ PLUS } 
	| '*' 				{ MULT } 
	| '-'				{ MINUS }
	| '~'				{ NEG }
	| "AND" 			{ AND } 
	| "OR" 				{ OR } 
	| "==" 				{ EQ } 
	| "="               { ASSIGN }
	| "!=" 				{ NEQ }
	| '<'				{ LESSTHAN }
	| "<="              { LESSEQTHAN }
	| '>'				{ GREATERTHAN }
	| ">="              { GREATEREQTHAN }
	| '.'				{ PROP_DEREF }
	| ','				{ COMMA }
	| ';'				{ SEMICOLON }
	| ':'				{ COLON }
	| '{'				{ LBRACE }
	| '}'				{ RBRACE }
	| '['				{ LBRACKET }
	| ']'				{ RBRACKET }
	| '(' 				{ LPAR }
	| ')' 				{ RPAR }
	| '?'				{ QUESTION }
	| '%'				{ EXCL }
	| '!'				{ PEND }
	| '|'				{ PIPE }
	| "=>" 				{ BOLDARROW }
	| "->" 				{ ARROW }
	| "String"			{ STRTY }
	| "Number" 			{ INTTY }
	| "Boolean" 		{ BOOLTY }
	| "List" 			{ LISTTY }
	| "tmpl" | "template" | "process" { TEMPLATE }
	| "when" 			{ WHEN }
	| "foreach"  		{ FOREACH }
	| "in"				{ IN }
	| "executed"		{ ID "executed" }
	| "included"		{ ID "included" }
	| "pending"			{ ID "pending" }
	| "value"			{ ID "value" }
	| int         		{ INT (int_of_string @@ Lexing.lexeme lexbuf) }
	| id 				{ ID (Lexing.lexeme lexbuf) }
	| eof 				{ EOL }
	| _ as tk 			{ error lexbuf "unknown token '%s'" @@ char_to_string tk}
	(* | eof 					    { raise End_of_file } *)


(* In doubt, see https://medium.com/@huund/recipes-for-ocamllex-bb4efa0afe53 *)
and read_string buffer 	= parse
	| '\''                  { Buffer.contents buffer } (* returns back to callee *)
	| '\\' 'n'  			{ Buffer.add_char buffer '\n'
								; read_string buffer lexbuf }
	| '\\' 'r'  			{ Buffer.add_char buffer '\r'
								; read_string buffer lexbuf }
	| '\\' 't'  			{ Buffer.add_char buffer '\t' 
								; read_string buffer lexbuf }
	| [^'\'' '\\']+			{ Buffer.add_string buffer @@ Lexing.lexeme lexbuf
								; read_string buffer lexbuf }
	(* | newline            { Buffer.add_string buf @@ Lexing.lexeme lexbuf
								; Lexing.new_line lexbuf ; read_string buf lexbuf } *)
	(* | '\\' '"'              { Buffer.add_char buffer '"'
								; read_string buffer lexbuf } *)
	(* | '\\'                  { Buffer.add_char buffer '\\'
								; read_string buffer lexbuf } *)
	| eof                   { error lexbuf "end of input inside of a string" }
	| _                     { error lexbuf "found '%s' - don't know how to handle" @@ Lexing.lexeme lexbuf }

and read_comment_block = parse
	| "###"					{ read_token lexbuf }
	| eof 					{ error lexbuf "end of input inside of a comment" }
	| _ 					{ read_comment_block lexbuf }

and read_line_comment = parse
	| newline | eof			{ Lexing.new_line lexbuf; read_token lexbuf }
	(* | eof 				{ raise End_of_file } *)
  	| _           			{ read_line_comment lexbuf }