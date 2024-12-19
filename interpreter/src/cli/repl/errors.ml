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
