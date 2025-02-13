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
          @@ Plain.unparse_expr e
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
    ( { location= Nowhere
      ; message= Printf.sprintf "Event %s is not enabled" (keyword id.data)
      ; hint=
          Some
            "Check any relations or conditions that might be blocking this event."
      }
    :: errors )

let param_not_found ?(errors = []) p =
  fail
    ( { location= p.loc
      ; message= Printf.sprintf "Parameter %s not found" (keyword p.data)
      ; hint= Some "Ensure the parameter is declared and in scope." }
    :: errors )

let invalid_expr ?(errors = []) e =
  fail
    ( { location= e.loc
      ; message= "Invalid expression"
      ; hint= Some "Ensure the expression is well-formed." }
    :: errors )

let unknown_error ?(errors = []) msg =
  fail
    ({location= Nowhere; message= "Unknown error: " ^ msg; hint= None} :: errors)
