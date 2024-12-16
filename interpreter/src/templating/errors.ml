open Syntax
open Lexing
open Unparser.PlainUnparser
open Misc.Printing
open Misc.Monads.ResultMonad
open Ppx_yojson_conv_lib.Yojson_conv

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Aux functions / type                                                     │
   └──────────────────────────────────────────────────────────────────────────┘ *)

type detailed_error = {location: loc; message: string; hint: string option}
[@@deriving yojson]

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ General errors                                                           │
   └──────────────────────────────────────────────────────────────────────────┘ *)

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

let rec property_not_found_type ?(errors = []) ?(loc = Nowhere) p ty =
  fail
    ( { location= loc
      ; message=
          "Property " ^ keyword p.data ^ " not found in " ^ keyword
          @@ unparse_ty ty
      ; hint=
          Some "Ensure the property is declared and in scope. Check for typos."
      }
    :: errors )

and id_not_found ?(errors = []) id =
  fail
    ( { location= id.loc
      ; message= "Identifier " ^ keyword id.data ^ " not found"
      ; hint=
          Some
            "Ensure the identifier is declared and in scope. Check for typos."
      }
    :: errors )

and tmpl_not_found ?(errors = []) ?(available = []) id =
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
    :: errors )

and duplicate_tmpl ?(errors = []) id =
  fail
    ( { location= id.loc
      ; message= "Duplicate template " ^ keyword id.data
      ; hint= Some "Ensure the template is not declared more than once." }
    :: errors )

and duplicate_event ?(errors = []) id =
  let line =
    match id.loc with Location (start, _, _) -> start.pos_lnum | _ -> 0
  in
  fail
    ( { location= id.loc
      ; message= Printf.sprintf "Duplicate event %s" (keyword id.data)
      ; hint=
          Some
            (Printf.sprintf "Event %s is already declared at line %s"
               (keyword id.data)
               (keyword (string_of_int line)) ) }
    :: errors )

and file_not_exists ?(errors = []) filename =
  fail
    ( { location= Nowhere
      ; message= Printf.sprintf "File %s does not exist" (keyword filename)
      ; hint= None }
    :: errors )

and invalid_file_extension ?(errors = []) ~supported ?(got = "") () =
  fail
    ( { location= Nowhere
      ; message= Printf.sprintf "Invalid file extension %s" (keyword got)
      ; hint= Some ("Supported extensions are " ^ keyword supported) }
    :: errors )

and excessive_exported_events ?(errors = []) ?(loc = Nowhere) x events =
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
    :: errors )

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Type errors                                                              │
   └──────────────────────────────────────────────────────────────────────────┘ *)

and is_not_type ?(errors = []) expected expr =
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
    :: errors )

and invalid_annotation_value ?(errors = []) value ty =
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
    :: errors )

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Expression errors                                                        │
   └──────────────────────────────────────────────────────────────────────────┘ *)

and invalid_expr ?(errors = []) ?(loc = Nowhere) () =
  fail
    ( { location= loc
      ; message= "Invalid expression"
      ; hint=
          Some
            "Check the syntax and structure of the expression. Ensure all definitions are correctly used."
      }
    :: errors )

and invalid_number_of_args ?(errors = []) ?(loc = Nowhere)
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
    :: errors )

and invalid_number_of_exported_events ?(errors = []) ?(loc = Nowhere) xs
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

and invalid_guard_value ?(errors = []) value =
  fail
    ( { location= value.loc
      ; message=
          Printf.sprintf
            "Invalid guard value. Expecting boolean expression, got %s"
            (keyword @@ unparse_expr value)
      ; hint= Some "Ensure the guard value is a boolean expression." }
    :: errors )

and value_from_input_event ?(errors = []) event =
  let id, _ = event.data.info in
  fail
    ( { location= event.loc
      ; message=
          Printf.sprintf "Value from input event %s is not allowed"
            (keyword id.data)
      ; hint= Some "Input events cannot have values. Check the event type." }
    :: errors )

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Event errors                                                             │
   └──────────────────────────────────────────────────────────────────────────┘ *)

and event_not_found ?(errors = []) ?(loc = Nowhere) id =
  fail
    ( { location= loc
      ; message= Printf.sprintf "Event %s not found" (keyword id)
      ; hint= Some "Ensure the event is declared and in scope. Check for typos."
      }
    :: errors )

and events_not_found ?(errors = []) ?(loc = Nowhere) ids =
  let string_ids =
    ids |> List.map (fun id -> keyword id.data) |> String.concat ", "
  in
  fail
    ( { location= loc
      ; message= Printf.sprintf "Events %s not found" string_ids
      ; hint=
          Some "Ensure the events are declared and in scope. Check for typos."
      }
    :: errors )

and event_not_enabled ?(errors = []) event =
  let id, _ = event.data.info in
  fail
    ( { location= event.loc
      ; message= Printf.sprintf "Event %s is not enabled" (keyword id.data)
      ; hint=
          Some
            "Check any relations or conditions that might be blocking this event."
      }
    :: errors )

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Command errors                                                           │
   └──────────────────────────────────────────────────────────────────────────┘ *)

and invalid_command ?(errors = []) ?nearest ?(distance = -1) cmd =
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

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Miscellaneous errors                                                     │
   └──────────────────────────────────────────────────────────────────────────┘ *)

and lexing_error ?(errors = []) lexbuf message =
  fail
    ( { location=
          Location
            ( lexbuf.lex_start_p
            , lexbuf.lex_curr_p
            , Some lexbuf.lex_curr_p.pos_fname )
      ; message= "lexing error, " ^ message
      ; hint=
          Some
            "Check the syntax near the location. Ensure all tokens are valid."
      }
    :: errors )

and syntax_error ?(errors = []) lexbuf =
  fail
    ( { location=
          Location
            ( lexbuf.lex_start_p
            , lexbuf.lex_curr_p
            , Some lexbuf.lex_curr_p.pos_fname )
      ; message= "Syntax error"
      ; hint=
          Some
            "Check the syntax near the location. Ensure all constructs are correctly formed."
      }
    :: errors )

and unexpected_eof ?(errors = []) lexbuf =
  fail
    ( { location=
          Location
            ( lexbuf.lex_start_p
            , lexbuf.lex_curr_p
            , Some lexbuf.lex_curr_p.pos_fname )
      ; message= "Unexpected end of file"
      ; hint=
          Some
            "Ensure the file is complete and all constructs are properly closed."
      }
    :: errors )

and unknown_error ?(errors = []) lexbuf =
  fail
    ( { location=
          Location
            ( lexbuf.lex_start_p
            , lexbuf.lex_curr_p
            , Some lexbuf.lex_curr_p.pos_fname )
      ; message= "Something went wrong..."
      ; hint=
          Some "An unknown error occurred. Report this issue in the repository."
      }
    :: errors )

and should_not_happen ?(errors = []) ?(module_path = "?") ?(line = "?") message
    =
  fail
    ( { location= Nowhere
      ; message= "This should not happen: " ^ message
      ; hint= Some (Printf.sprintf "Check module %s, line %s" module_path line)
      }
    :: errors )

and something_went_wrong ?(loc = Nowhere) message = fixme ~loc message

and invalid_logger_level level =
  fail
    [ { location= Nowhere
      ; message= Printf.sprintf "Invalid logger level %s" (keyword level)
      ; hint= None } ]

and todo ?(loc = Nowhere) message =
  fail
    [ { location= loc
      ; message= CString.colorize ~color:Cyan ~format:Bold "[todo] " ^ message
      ; hint= None } ]

and fixme ?(loc = Nowhere) message =
  fail
    [ { location= loc
      ; message= CString.colorize ~color:Red ~format:Bold "[fixme] " ^ message
      ; hint= None } ]

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Typechecker errors                                                       │
   └──────────────────────────────────────────────────────────────────────────┘ *)

and type_mismatch ?(errors = []) ?(loc = Nowhere) expected_tys got_tys =
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

and event_type_mismatch ?(errors = []) ?(loc = Nowhere) ?(available = [])
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
      (keyword
         (show_event_type' (Unparser.PlainUnparser.unparse_ty ty) event_type) )
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
                         (show_event_type'
                            (Unparser.PlainUnparser.unparse_ty ty)
                            event_type ) )
                     available ) ) ) }
    :: errors )

and missing_label ?(errors = []) ?(available_labels = []) label =
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

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Error printing                                                           │
   └──────────────────────────────────────────────────────────────────────────┘ *)

let get_line_content filepath line =
  let file = open_in filepath in
  let rec trim_prefix str indent =
    match (str, indent) with
    | "", i -> ("", i)
    | str, i when str.[0] = ' ' ->
        trim_prefix (String.sub str 1 (String.length str - 1)) (i + 1)
    | str -> str
  in
  let rec read_line n =
    match input_line file with
    | content when n = line -> content
    | _ -> read_line (n + 1)
  in
  let line_content = read_line 1 in
  close_in file ; trim_prefix line_content 0

let extract_location_info loc =
  match loc with
  | Nowhere -> (None, 0, 0, 0)
  | Location (start_pos, end_pos, filename) ->
      let line = start_pos.Lexing.pos_lnum in
      let start_char = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
      let end_char = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol in
      (filename, line, start_char, end_char)

let error_pointer = '^'

let pretty_string_error detailed_error =
  let {location; message; hint} = detailed_error in
  let message_header =
    CString.colorize ~format:Bold ~color:Red "error: " ^ message ^ "\n"
  and blue_text = CString.colorize ~color:Blue
  and highlight_section (s, e) ~fmt text =
    let prefix = String.sub text 0 s in
    let highlight = String.sub text s (e - s) in
    let suffix = String.sub text e (String.length text - e) in
    Printf.sprintf "%s%s%s" prefix (fmt highlight) suffix
  in
  let message_file_section =
    let filepath, line, start_char, end_char = extract_location_info location in
    let line_size = String.length (string_of_int line) in
    let line_margin = String.make line_size ' ' in
    let marker ?(indent = 0) _ =
      String.concat ""
        [ String.make (start_char + indent) ' '
        ; CString.colorize ~color:Red ~format:Bold
            (String.make (end_char - start_char) error_pointer) ]
    in
    match filepath with
    | None -> ""
    | Some filepath when Sys.file_exists filepath ->
        let file_header =
          CString.colorize ~format:Bold
          @@ Printf.sprintf "%s:%d:%d" filepath line (start_char + 1)
        and file_gutter ?number content =
          let number_str =
            if Option.is_none number then line_margin
            else Option.get number |> string_of_int
          in
          blue_text ~format:Bold (Printf.sprintf " %s | " number_str) ^ content
        in
        let line_content, indent = get_line_content filepath line in
        let line_content =
          highlight_section (start_char, end_char)
            ~fmt:(CString.colorize ~format:Bold)
            line_content
        in
        Printf.sprintf "%s\n%s\n%s\n%s\n\n" file_header (file_gutter "")
          (file_gutter ~number:line line_content)
          (file_gutter (marker ~indent:(-indent) ()))
    | Some filepath ->
        blue_text
        @@ Printf.sprintf "at %s:%d:%d\n\n" filepath line (start_char + 1)
  in
  let message_hint =
    match hint with
    | None -> ""
    | Some message ->
        CString.colorize ~format:Bold ~color:Cyan "hint: " ^ message ^ "\n"
  in
  Printf.sprintf "%s%s%s" message_header message_file_section message_hint

let pretty_string_errors errors =
  List.map pretty_string_error errors |> String.concat "\n"

(*┌*)
let print_error detailed_error =
  let result = pretty_string_error detailed_error in
  print_endline result

let print_errors errors = List.iter print_error errors
