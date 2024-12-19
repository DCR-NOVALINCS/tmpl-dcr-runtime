(* open Dcr
   open Syntax
   open Lexing
   open Unparser.PlainUnparser
   open Ppx_yojson_conv_lib.Yojson_conv
   open Common
   open Location
   open Monads.ResultMonad
   open Printing *)

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Aux functions / type                                                     │
   └──────────────────────────────────────────────────────────────────────────┘ *)

(* type detailed_error = {location: loc; message: string; hint: string option}
   [@@deriving yojson] *)

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ General errors                                                           │
   └──────────────────────────────────────────────────────────────────────────┘ *)

(* and id_not_found ?(errors = []) id =
   fail
     ( { location= id.loc
       ; message= "Identifier " ^ keyword id.data ^ " not found"
       ; hint=
           Some
             "Ensure the identifier is declared and in scope. Check for typos."
       }
     :: errors ) *)

(* and tmpl_not_found ?(errors = []) ?(available = []) id =
   let available_tmpls =
     List.map (fun tmpl_id -> Printf.sprintf " - %s" (keyword tmpl_id)) available
     |> String.concat "\n"
   in
   fail
     ( { location= id.loc
       ; message= "Template " ^ keyword id.data ^ " not found"
       ; hint=
           ( match available with
           | [] -> Some "Check for typos in the template name."
           | _ ->
               Some (Printf.sprintf "Available templates:\n%s" available_tmpls)
           ) }
     :: errors ) *)

(* and excessive_exported_events ?(errors = []) ?(loc = Nowhere) x events =
   let string_xs =
     x |> List.map (fun id -> Printf.sprintf "%s" id.data) |> String.concat ", "
   in
   let string_event_ids =
     events
     |> List.map (fun event -> Printf.sprintf "%s" (fst event.data.info).data)
     |> String.concat ", "
   in
   fail
     ( { location= loc
       ; message= Printf.sprintf "Excessive exported events %s" string_xs
       ; hint=
           Some
             (Printf.sprintf "Events %s that can be exported" string_event_ids)
       }
     :: errors ) *)

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Type errors                                                              │
   └──────────────────────────────────────────────────────────────────────────┘ *)

(* and is_not_type ?(errors = []) expected expr =
   fail
     ( { location= expr.loc
       ; message=
           Printf.sprintf "Expected type %s, but got %s" (keyword expected)
             (keyword @@ unparse_expr expr)
       ; hint=
           Some
             ( "Verify the type of the expression "
             ^ (keyword @@ unparse_expr expr)
             ^ ". Check for type mismatches or any typos." ) }
     :: errors ) *)

(* and invalid_annotation_value ?(errors = []) value ty =
   fail
     ( { location= value.loc
       ; message=
           Printf.sprintf "Invalid annotation value %s for type %s"
             (keyword @@ unparse_expr value)
             (keyword @@ unparse_ty ty)
       ; hint=
           Some
             "Verify the annotation value matches the expected type. Check for type mismatches or any typos."
       }
     :: errors ) *)

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Expression errors                                                        │
   └──────────────────────────────────────────────────────────────────────────┘ *)

(* and invalid_expr ?(errors = []) ?(loc = Nowhere) () =
   fail
     ( { location= loc
       ; message= "Invalid expression"
       ; hint=
           Some
             "Check the syntax and structure of the expression. Ensure all definitions are correctly used."
       }
     :: errors ) *)

(* and invalid_number_of_args ?(errors = []) ?(loc = Nowhere)
     ?(missing_params = []) () =
   let string_missing =
     missing_params
     |> List.map (fun (param, ty) ->
            Printf.sprintf "%s of type %s" (keyword param.data)
              (keyword @@ unparse_ty ty) )
     |> String.concat ", "
   in
   fail
     ( { location= loc
       ; message=
           Printf.sprintf "Invalid number of arguments. Missing parameters %s"
             string_missing
       ; hint= None }
     :: errors ) *)

(* and invalid_guard_value ?(errors = []) value =
   fail
     ( { location= value.loc
       ; message=
           Printf.sprintf
             "Invalid guard value. Expecting boolean expression, got %s"
             (keyword @@ unparse_expr value)
       ; hint= Some "Ensure the guard value is a boolean expression." }
     :: errors ) *)

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Event errors                                                             │
   └──────────────────────────────────────────────────────────────────────────┘ *)

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Command errors                                                           │
   └──────────────────────────────────────────────────────────────────────────┘ *)

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Miscellaneous errors                                                     │
   └──────────────────────────────────────────────────────────────────────────┘ *)

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Typechecker errors                                                       │
   └──────────────────────────────────────────────────────────────────────────┘ *)
