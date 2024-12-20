open Ast.Error
open Ast.Syntax
open Common
open Monads.ResultMonad
open Printing

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
