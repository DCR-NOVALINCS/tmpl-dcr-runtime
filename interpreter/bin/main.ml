open Cli

type (+'a, 'b) _source =
  { filename: string
  ; options: 'a option
  ; errors: ('a, 'b) Result.t list
  ; result: 'a option }

let _empty_source = {filename= ""; options= None; errors= []; result= None}

let _is_string filename = (filename :> string)

let start =
  if !Sys.interactive then (
    print_endline (_is_string "siui") ;
    () )
  else Main.run ()

let () = start
