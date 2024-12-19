open Ast.Error
open Ast.Syntax
open Common
open Monads.ResultMonad
open Printing

let invalid_logger_level level =
  fail
    [ { location= Nowhere
      ; message= Printf.sprintf "Invalid logger level %s" (keyword level)
      ; hint= None } ]

let invalid_command ?(errors = []) ?nearest ?(distance = -1) cmd =
  let guess =
    match nearest with
    | Some n when distance > 0 ->
        "Did you mean " ^ CString.colorize ~color:Green n ^ "?"
    | Some _ when distance = 0 -> "Did you miss some parameters?"
    | _ -> ""
  in
  fail
    ( { location= Nowhere
      ; message=
          Printf.sprintf "Invalid command %s" (String.concat " " cmd |> keyword)
      ; hint=
          Some
            ( guess ^ " Type "
            ^ CString.colorize ~color:Green "help"
            ^ " to see the available commands." ) }
    :: errors )

let file_not_exists ?(errors = []) filename =
  fail
    ( { location= Nowhere
      ; message= Printf.sprintf "File %s does not exist" (keyword filename)
      ; hint= None }
    :: errors )

let invalid_file_extension ?(errors = []) ~supported ?(got = "") () =
  fail
    ( { location= Nowhere
      ; message= Printf.sprintf "Invalid file extension %s" (keyword got)
      ; hint= Some ("Supported extensions are " ^ keyword supported) }
    :: errors )
