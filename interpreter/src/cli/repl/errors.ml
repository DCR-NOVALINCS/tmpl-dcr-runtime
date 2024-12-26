open Ast.Error
open Ast.Syntax
open Common
open Monads.ResultMonad
open Printing

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

let invalid_export_mode ?(errors = []) mode =
  fail
    ( { location= Nowhere
      ; message= Printf.sprintf "Invalid mode %s" (keyword mode)
      ; hint= None }
    :: errors )

let invalid_view_mode ?(errors = []) mode =
  fail
    ( { location= Nowhere
      ; message= Printf.sprintf "Invalid mode %s" (keyword mode)
      ; hint= None }
    :: errors )

let invalid_view_combination ?(errors = []) ?hint mode1 mode2 =
  fail
    ( { location= Nowhere
      ; message=
          Printf.sprintf "Invalid combination of modes %s and %s"
            (keyword mode1) (keyword mode2)
      ; hint }
    :: errors )

let invalid_number_rollback ?(errors = []) num =
  fail
    ( { location= Nowhere
      ; message= "Invalid number " ^ keyword (string_of_int num)
      ; hint= None }
    :: errors )
