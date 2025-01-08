open Ppx_yojson_conv_lib.Yojson_conv
open Syntax
open Common
open Printing
open Monads.ResultMonad

type detailed_error = {location: loc; message: string; hint: string option}
[@@deriving yojson]

(* ┌──────────────────────────────────────────────────────────────────────────┐
   │ Common Errors                                                            │
   └──────────────────────────────────────────────────────────────────────────┘ *)

let todo ?(loc = Nowhere) message =
  fail
    [ { location= loc
      ; message= CString.colorize ~color:Cyan ~format:Bold "[todo] " ^ message
      ; hint= None } ]

let fixme ?(loc = Nowhere) message =
  fail
    [ { location= loc
      ; message= CString.colorize ~color:Red ~format:Bold "[fixme] " ^ message
      ; hint= None } ]

let something_went_wrong ?(loc = Nowhere) message = fixme ~loc message

let should_not_happen ?(errors = []) ?(loc = Nowhere) ?(module_path = "?")
    ?(line = "?") message =
  fail
    ( { location= loc
      ; message= "This should not happen: " ^ message
      ; hint= Some (Printf.sprintf "Check module %s, line %s" module_path line)
      }
    :: errors )

let event_not_found ?(errors = []) ?(loc = Nowhere) id =
  fail
    ( { location= loc
      ; message= Printf.sprintf "Event %s not found" (keyword id)
      ; hint= Some "Ensure the event is declared and in scope. Check for typos."
      }
    :: errors )

let events_not_found ?(errors = []) ?(loc = Nowhere) ids =
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

let id_not_found ?(errors = []) id =
  fail
    ( { location= id.loc
      ; message= "Identifier " ^ keyword id.data ^ " not found"
      ; hint=
          Some
            "Ensure the identifier is declared and in scope. Check for typos."
      }
    :: errors )

let tmpl_not_found ?(errors = []) ?(available = []) id =
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

let duplicate_tmpl ?(errors = []) id =
  fail
    ( { location= id.loc
      ; message= "Duplicate template " ^ keyword id.data
      ; hint= Some "Ensure the template is not declared more than once." }
    :: errors )

let duplicate_event ?(errors = []) id =
  fail
    ( { location= id.loc
      ; message= Printf.sprintf "Duplicate event %s" (keyword id.data)
      ; hint= Some "Ensure the event is not declared more than once." }
    :: errors )

let value_from_input_event ?(errors = []) event =
  let id, _ = event.data.info in
  fail
    ( { location= event.loc
      ; message=
          Printf.sprintf "Value from input event %s is not allowed"
            (keyword id.data)
      ; hint= Some "Input events cannot have values. Check the event type." }
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
  and blue_text =
    CString.colorize ~color:Blue
    (* and highlight_section (s, e) ~fmt text =
       let prefix = String.sub text 0 s in
       let highlight = String.sub text s (e - s) in
       let suffix = String.sub text e (String.length text - e - 1) in
       Printf.sprintf "%s%s%s" prefix (fmt highlight) suffix *)
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
        (* let line_content =
             highlight_section
               (start_char - indent, end_char - indent)
               ~fmt:(CString.colorize ~format:Bold)
               line_content
           in *)
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
