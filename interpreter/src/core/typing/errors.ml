open Ast
open Syntax
open Error
open Unparser
open Common
open Monads.ResultMonad
open Printing

let type_mismatch ?(errors = []) ?(loc = Nowhere) expected_tys got_tys =
  (* let string_expected = unparse_ty expected in *)
  let rec string_tys = function
    | [] -> keyword "?"
    | [ty1] -> keyword (unparse_ty ty1)
    | ty1 :: [last] ->
        keyword (unparse_ty ty1) ^ " and " ^ keyword (unparse_ty last)
    | ty :: tys -> keyword (unparse_ty ty) ^ ", " ^ string_tys tys
  in
  fail
    ( { location= loc
      ; message=
          Printf.sprintf "Type mismatch. Expected %s, but got %s"
            (string_tys expected_tys) (string_tys got_tys)
      ; hint=
          Some
            "Verify the types of the expressions. Check for type mismatches or any typos."
      }
    :: errors )

let event_type_mismatch ?(errors = []) ?(loc = Nowhere) ?(available = [])
    expected_ty got_ty =
  let rec string_tys = function
    | [] -> keyword "?"
    | [(event_label, event_type, ty)] ->
        string_of_event_type_pair (event_label, event_type, ty)
    | (event_label, event_type, ty)
      :: [(event_label_last, event_type_last, ty_last)] ->
        Printf.sprintf "%s and %s"
          (string_of_event_type_pair (event_label, event_type, ty))
          (string_of_event_type_pair
             (event_label_last, event_type_last, ty_last) )
    | (event_label, event_type, ty) :: tys ->
        Printf.sprintf "%s, %s"
          (string_of_event_type_pair (event_label, event_type, ty))
          (string_tys tys)
  and string_of_event_type_pair (event_label, event_type, ty) =
    Printf.sprintf "%s%s" (keyword event_label)
      (keyword (show_event_type' (unparse_ty ty) event_type))
    (* (keyword ) *)
  in
  fail
    ( { location= loc
      ; message=
          Printf.sprintf "Event type mismatch. Expected %s, but got %s"
            (keyword @@ string_tys expected_ty)
            (keyword @@ string_tys got_ty)
      ; hint=
          Some
            (Printf.sprintf
               "Verify the type of the event. Check for type mismatches or any typos.\n\nAvailable event types:\n%s"
               (String.concat "\n"
                  (List.map
                     (fun (event_label, (ty, event_type)) ->
                       Printf.sprintf "- %s: %s" event_label
                         (show_event_type' (unparse_ty ty) event_type) )
                     available ) ) ) }
    :: errors )

let missing_label ?(errors = []) ?(available_labels = []) label =
  fail
    ( { location= label.loc
      ; message= Printf.sprintf "Missing label %s" (keyword label.data)
      ; hint=
          Some
            ( "Ensure the label is declared and in scope. Check for typos."
            ^
            match available_labels with
            | [] -> ""
            | _ ->
                let available_labels_str =
                  List.map
                    (fun label -> Printf.sprintf " - %s" (keyword label.data))
                    available_labels
                  |> String.concat "\n"
                in
                Printf.sprintf "Available labels:\n%s" available_labels_str ) }
    :: errors )

let property_not_found_type ?(errors = []) ?(loc = Nowhere) p ty =
  fail
    ( { location= loc
      ; message=
          "Property " ^ keyword p.data ^ " not found in " ^ keyword
          @@ unparse_ty ty
      ; hint=
          Some "Ensure the property is declared and in scope. Check for typos."
      }
    :: errors )
