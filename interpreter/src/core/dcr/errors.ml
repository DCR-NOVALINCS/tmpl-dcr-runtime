open Ast
open Syntax
open Error
open Unparser
open Common
open Monads.ResultMonad
open Printing

let property_not_found ?(errors = []) p e =
  fail
    ( { location= e.loc
      ; message=
          "Property " ^ keyword p.data ^ " not found in " ^ keyword
          @@ unparse_expr e
      ; hint=
          Some "Ensure the property is declared and in scope. Check for typos."
      }
    :: errors )

let invalid_number_of_exported_events ?(errors = []) ?(loc = Nowhere) xs
    exported =
  fail
    ( { location= loc
      ; message=
          Printf.sprintf
            "Invalid number of exported events. Expected %s, but got %s"
            (keyword (Printf.sprintf "%d" @@ List.length exported))
            (keyword (Printf.sprintf "%d" @@ List.length xs))
      ; hint=
          Some
            "Ensure the number of exported events matches the number of events in the program."
      }
    :: errors )

let event_not_enabled ?(errors = []) event =
  let id, _ = event.data.info in
  fail
    ( { location= event.loc
      ; message= Printf.sprintf "Event %s is not enabled" (keyword id.data)
      ; hint=
          Some
            "Check any relations or conditions that might be blocking this event."
      }
    :: errors )
