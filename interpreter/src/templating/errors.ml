open Syntax
open Lexing
open Unparser.PlainUnparser
open Misc.Printing
open Misc.Monads.ResultMonad
open Ppx_yojson_conv_lib.Yojson_conv

type detailed_error = {location: loc; message: string; hint: string option}
[@@deriving yojson]

(* =============================================================================
   General errors
   ============================================================================= *)

let property_not_found ?(errors = []) p e =
  fail
    ( { location= e.loc
      ; message=
          "Property "
          ^ CString.colorize ~color:Yellow p.data
          ^ " not found in "
          ^ CString.colorize ~color:Yellow
          @@ unparse_expr e
      ; hint=
          Some "Ensure the property is declared and in scope. Check for typos."
      }
    :: errors )

let property_not_found_type ?(errors = []) ?(loc = Nowhere) p ty =
  fail
    ( { location= loc
      ; message=
          "Property "
          ^ CString.colorize ~color:Yellow p.data
          ^ " not found in "
          ^ CString.colorize ~color:Yellow
          @@ unparse_ty ty
      ; hint=
          Some "Ensure the property is declared and in scope. Check for typos."
      }
    :: errors )

and id_not_found ?(errors = []) id =
  fail
    ( { location= id.loc
      ; message=
          "Identifier " ^ CString.colorize ~color:Yellow id.data ^ " not found"
      ; hint=
          Some
            "Ensure the identifier is declared and in scope. Check for typos."
      }
    :: errors )

and tmpl_not_found ?(errors = []) ?(available = []) id =
  let available_tmpls =
    List.map
      (fun tmpl_id ->
        Printf.sprintf " - %s" (CString.colorize ~color:Yellow tmpl_id) )
      available
    |> String.concat "\n"
  in
  fail
    ( { location= id.loc
      ; message=
          "Template " ^ CString.colorize ~color:Yellow id.data ^ " not found"
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
      ; message= "Duplicate template " ^ CString.colorize ~color:Yellow id.data
      ; hint= Some "Ensure the template is not declared more than once." }
    :: errors )

and file_not_exists ?(errors = []) filename =
  fail
    ( { location= Nowhere
      ; message=
          Printf.sprintf "File %s does not exist"
            (CString.colorize ~color:Yellow filename)
      ; hint= None }
    :: errors )

and invalid_file_extension ?(errors = []) ~supported ?(got = "") () =
  fail
    ( { location= Nowhere
      ; message=
          Printf.sprintf "Invalid file extension %s"
            (CString.colorize ~color:Yellow got)
      ; hint=
          Some
            ( "Supported extensions are "
            ^ CString.colorize ~color:Yellow supported ) }
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

(* =============================================================================
   Type errors
   ============================================================================= *)

and is_not_type ?(errors = []) expected expr =
  fail
    ( { location= expr.loc
      ; message=
          Printf.sprintf "Expected type %s, but got %s"
            (CString.colorize ~color:Yellow expected)
            (CString.colorize ~color:Yellow @@ unparse_expr expr)
      ; hint=
          Some
            ( "Verify the type of the expression "
            ^ (CString.colorize ~color:Yellow @@ unparse_expr expr)
            ^ ". Check for type mismatches or any typos." ) }
    :: errors )

and invalid_annotation_value ?(errors = []) value ty =
  fail
    ( { location= value.loc
      ; message=
          Printf.sprintf "Invalid annotation value %s for type %s"
            (CString.colorize ~color:Yellow @@ unparse_expr value)
            (CString.colorize ~color:Yellow @@ unparse_ty ty)
      ; hint=
          Some
            "Verify the annotation value matches the expected type. Check for type mismatches or any typos."
      }
    :: errors )

(* =============================================================================
   Expression errors
   ============================================================================= *)

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
           Printf.sprintf "%s of type %s"
             (CString.colorize ~color:Yellow param.data)
             (CString.colorize ~color:Yellow @@ unparse_ty ty) )
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
            (CString.colorize ~color:Yellow
               (Printf.sprintf "%d" @@ List.length exported) )
            (CString.colorize ~color:Yellow
               (Printf.sprintf "%d" @@ List.length xs) )
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
            (CString.colorize ~color:Yellow @@ unparse_expr value)
      ; hint= Some "Ensure the guard value is a boolean expression." }
    :: errors )

(* =============================================================================
   Event errors
   ============================================================================= *)

and event_not_found ?(errors = []) ?(loc = Nowhere) id =
  fail
    ( { location= loc
      ; message=
          Printf.sprintf "Event %s not found"
            (CString.colorize ~color:Yellow id)
      ; hint= Some "Ensure the event is declared and in scope. Check for typos."
      }
    :: errors )

and event_not_enabled ?(errors = []) event =
  let id, _ = event.data.info in
  fail
    ( { location= event.loc
      ; message=
          Printf.sprintf "Event %s is not enabled"
            (CString.colorize ~color:Yellow id.data)
      ; hint=
          Some
            "Check any relations or conditions that might be blocking this event."
      }
    :: errors )

(* =============================================================================
   Command errors
   ============================================================================= *)

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
          Printf.sprintf "Invalid command %s"
            (String.concat " " cmd |> CString.colorize ~color:Yellow)
      ; hint=
          Some
            ( guess ^ " Type "
            ^ CString.colorize ~color:Green "help"
            ^ " to see the available commands." ) }
    :: errors )

(* =============================================================================
   Miscellaneous errors
   ============================================================================= *)

and lexing_error ?(errors = []) lexbuf message =
  fail
    ( { location=
          Location
            ( lexbuf.lex_start_p
            , lexbuf.lex_curr_p
            , Some lexbuf.lex_curr_p.pos_fname )
      ; message= "Lexing fail: " ^ message
      ; hint=
          Some
            "Check the syntax near the fail location. Ensure all tokens are valid."
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
            "Check the syntax near the fail location. Ensure all constructs are correctly formed."
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
          Some "An unknown fail occurred. Report this issue in the repository."
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

and todo ?(loc = Nowhere) message =
  fail
    [ { location= loc
      ; message= CString.colorize ~color:Cyan "[todo] " ^ message
      ; hint= None } ]

and fixme ?(loc = Nowhere) message =
  fail
    [ { location= loc
      ; message= CString.colorize ~color:Yellow "[fixme] " ^ message
      ; hint= None } ]

(* =============================================================================
   Typechecker errors
   ============================================================================= *)

and type_mismatch ?(errors = []) ?(loc = Nowhere) expected_tys got_tys =
  (* let string_expected = unparse_ty expected in *)
  let rec string_tys = function
    | [] -> CString.colorize ~color:Yellow "?"
    | [ty1] -> CString.colorize ~color:Yellow (unparse_ty ty1)
    | ty1 :: [last] ->
        CString.colorize ~color:Yellow (unparse_ty ty1)
        ^ " and "
        ^ CString.colorize ~color:Yellow (unparse_ty last)
    | ty :: tys ->
        CString.colorize ~color:Yellow (unparse_ty ty) ^ ", " ^ string_tys tys
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

(* =============================================================================
   Error printing
   ============================================================================= *)

let get_line_content filepath line =
  if Sys.file_exists filepath then (
    let file = open_in filepath in
    let rec read_line n =
      match input_line file with
      | content when n = line -> content
      | _ -> read_line (n + 1)
    in
    let line_content = read_line 1 in
    close_in file ; line_content )
  else ""

let extract_location_info loc =
  match loc with
  | Nowhere -> (None, 0, 0, 0)
  | Location (start_pos, end_pos, filename) ->
      let line = start_pos.Lexing.pos_lnum in
      let start_char = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
      let end_char = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol in
      (filename, line, start_char, end_char)

(*┌*)
let print_error detailed_error =
  let {location; message; hint} = detailed_error in
  let message_header = CPrinter.eprint "error: " ; CPrinter.cprintln message in
  let message_file_section =
    let filepath, line, start_char, end_char = extract_location_info location in
    let line_size = String.length (string_of_int line) in
    let line_margin = String.make line_size ' ' in
    let marker =
      String.concat ""
        [ String.make start_char ' '
        ; CString.colorize ~color:Red (String.make (end_char - start_char) '^')
        ]
    in
    match filepath with
    | None -> ()
    | Some filepath ->
        let line_content = get_line_content filepath line in
        CPrinter.cprintf " %s:%d:%d\n" filepath line (start_char + 1) ;
        CPrinter.cprintf " %s│\n" line_margin ;
        CPrinter.cprintf "%d │ %s\n" line line_content ;
        CPrinter.cprintf " %s│ %s" line_margin marker ;
        CPrinter.cprintln ""
  in
  let message_hint =
    match hint with
    | None -> ()
    | Some message ->
        CPrinter.cprint ~color:Cyan "hint: " ;
        CPrinter.cprintln message
  in
  message_header ; message_file_section ; message_hint
(* CPrinter.cprintln "" *)
