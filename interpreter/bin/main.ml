open Templating
open Syntax
open Api
open Instantiation
open Lex_and_parse
open Errors
open Typechecking
open Unparser
open Program_helper
open Misc
open Env
open Monads.ResultMonad
open Printing

(* open Misc.Env *)

(* =============================================================================
   Aux functions
   ============================================================================= *)

let start_header =
  CPrinter.cprint "To get started, type " ;
  CPrinter.cprint ~color:Green "help" ;
  CPrinter.cprint " or " ;
  CPrinter.cprint ~color:Green "h" ;
  CPrinter.cprintln " to see the available commands.\n"

let get_file_extension filename =
  let parts = String.split_on_char '.' filename in
  List.nth parts (List.length parts - 1)

let input_file_extension = "tdcr"

let input_file filename =
  get_file_extension filename
  |> function
  | ext when ext = input_file_extension -> return ()
  | got -> invalid_file_extension ~supported:input_file_extension ~got ()

(* =============================================================================
   Runtime State Management Section
   ============================================================================= *)

type runtime_state =
  { ty_env: type_expr' env
  ; expr_env: expr env
  ; event_env: event env
  ; program: program
  ; output: string }

let mk_runtime_state ?(output = "") ?(ty_env = empty_env)
    ?(expr_env = empty_env) ?(event_env = empty_env) program =
  {ty_env; expr_env; event_env; program; output}

let empty_runtime_state =
  mk_runtime_state ~output:"" ~ty_env:empty_env ~expr_env:empty_env
    ~event_env:empty_env empty_program

let print_output ?(previous_state = empty_runtime_state) = function
  | Ok runtime_state ->
      let {output; _} = runtime_state in
      CPrinter.cprintln output ; return runtime_state
  | Error errors ->
      (* Logger.error "Errors found" ; *)
      print_errors errors ; return previous_state

let sanitize_input input =
  input |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") |> return

let help_message cmds =
  let header = CString.colorize ~color:BrightCyan "Available Commands:" in
  let cmds_section =
    cmds
    |> List.map (fun {name; alias; params; desc} ->
           Printf.sprintf "- %s (%s) %s : %s"
             (CString.colorize ~color:Green name)
             (CString.colorize ~color:Green alias)
             (String.concat " " (List.map (CString.colorize ~color:Red) params))
             desc )
    |> String.concat "\n"
  in
  String.concat "\n" [header; cmds_section]

let parse_expression expr_string =
  let expr = expr_string |> String.concat " " in
  let expr_lexbuf = Lexing.from_string expr in
  Logger.debug @@ "Parsing expression " ^ expr ;
  parse_expression expr_lexbuf

let parse filename =
  if Sys.file_exists filename then (
    Logger.debug @@ "Reading file: " ^ CString.colorize ~color:Yellow filename ;
    let lexbuf = Lexing.from_channel (open_in filename) in
    let prog = parse_program ~filename lexbuf in
    prog )
  else file_not_exists filename

let get_program =
  if Array.length Sys.argv < 2 then return empty_program
  else
    let filename = Sys.argv.(1) in
    input_file filename >>= fun _ -> parse filename

(* =============================================================================
   Main Section
   ============================================================================= *)

let read_command tokens program (ty_env, expr_env, event_env) =
  Logger.debug @@ "Command: " ^ String.concat " | " tokens ;
  match tokens with
  | ["exit"] | ["q"] -> exit 0
  | ["help"] | ["h"] ->
      (* return (program, help_message cmds) *)
      return
        (mk_runtime_state ~ty_env ~expr_env ~event_env
           ~output:(help_message cmds) program )
  | ["parse"; filename] | ["p"; filename] ->
      input_file filename
      >>= fun _ ->
      parse filename
      >>= fun program ->
      return
        (mk_runtime_state ~ty_env ~expr_env ~event_env
           ~output:"Program parsed successfully" program )
  | "exec" :: event_id :: expr | "e" :: event_id :: expr ->
      ( if expr = [] then
          return
            (annotate ~ty:(Some UnitTy)
               ~loc:(mk_loc Lexing.dummy_pos Lexing.dummy_pos)
               Unit )
        else parse_expression expr )
      >>= fun parsed_expr ->
      Logger.debug "Event env: \n" ;
      Logger.debug
      @@ string_of_env
           (fun e -> Unparser.PlainUnparser.unparse_events [e])
           event_env ;
      execute ~event_id ~expr:parsed_expr.data ~ty_env ~expr_env ~event_env
        program
      >>= fun (program, event_env, expr_env) ->
      return
        (mk_runtime_state ~ty_env ~expr_env ~event_env
           ~output:
             ( "Event executed with expression "
             ^ CString.colorize ~color:Yellow
             @@ PlainUnparser.unparse_expr parsed_expr )
           program )
      (* ( program , "Event executed with expression " ^ CString.colorize
         ~color:Yellow @@ PlainUnparser.unparse_expr parsed_expr ) *)
  | ["view"] | ["v"] ->
      view_enabled program
      >>= fun unparsed_program ->
      (* return (program, unparsed_program) *)
      return
        (mk_runtime_state ~ty_env ~expr_env ~event_env ~output:unparsed_program
           program )
  | ["debug"] | ["d"] ->
      view_debug program
      >>= fun unparsed_program ->
      (* return (program, unparsed_program) *)
      return
        (mk_runtime_state ~ty_env ~expr_env ~event_env ~output:unparsed_program
           program )
  | "dino" :: "says" :: message ->
      let dinossaur message =
        Printf.sprintf
          "               [31m__[0m\n\              [32m/ _)  - %s[0m\n\     [33m_.----._/ /[0m\n\    [34m/         /[0m\n\ [35m__/ (  | (  |[0m\n[36m/__.-'|_|--|_|[0m\n"
          message
      in
      (* return (program, dinossaur @@ String.concat " " message) *)
      return
        (mk_runtime_state ~ty_env ~expr_env ~event_env
           ~output:(dinossaur @@ String.concat " " message)
           program )
  | "export" :: filenames | "exp" :: filenames ->
      (* FIXME: add specific function to do this *)
      (* view_debug program  *)
      unparse_program program
      >>= fun unparsed_program ->
      let write_to_file filename =
        let oc = open_out filename in
        Printf.fprintf oc "%s\n" unparsed_program ;
        close_out oc
      in
      List.iter write_to_file filenames ;
      (* return ( program , "Program exported to " ^ CString.colorize
         ~color:Yellow (String.concat ", " filenames) ) *)
      return
        (mk_runtime_state ~ty_env ~expr_env ~event_env
           ~output:
             ( "Program exported to "
             ^ CString.colorize ~color:Yellow (String.concat ", " filenames) )
           program )
  | [] ->
      (* return (program, "") *)
      return (mk_runtime_state ~ty_env ~expr_env ~event_env program)
  | _ ->
      let open Misc.Bktree in
      let line = String.concat " " tokens in
      let nearest, distance =
        nearest_neighbor levenshtein_distance cmds_bbk_tree line
      in
      todo "read_line"
      >>= fun _ ->
      todo "read_line"
      >>= fun _ ->
      todo "read_line" >>= fun _ -> invalid_command ~nearest ~distance tokens

let rec prompt program (ty_env, expr_env, event_env) =
  CPrinter.cprint ~color:BrightGreen "> " ;
  (* Read line *)
  sanitize_input @@ read_line ()
  >>= fun tokens ->
  (* Process command *)
  read_command tokens program (ty_env, expr_env, event_env)
  |> print_output
       ~previous_state:(mk_runtime_state ~ty_env ~expr_env ~event_env program)
  >>= fun {ty_env; expr_env; event_env; program; _} ->
  (* Continue... *)
  prompt program (ty_env, expr_env, event_env)

let runtime =
  (* Logger settings *)
  Logger.enable () ;
  Logger.set_logger_level Warn ;
  (* Get & Parse the initial input *)
  get_program
  >>= (fun program ->
        (* Preprocess program *)
        Logger.info "Program parsed successfully" ;
        preprocess_program program
        >>= fun (event_env, expr_env, program) ->
        (* Typecheck program *)
        typecheck ~event_env program
        >>= fun (ty_env, event_env) ->
        Logger.info "Program typechecked successfully" ;
        (* Instantiate initial templates *)
        instantiate ~expr_env ~event_env program
        >>= fun (program, event_env, expr_env) ->
        Logger.info "Program instantiated successfully" ;
        (* Display welcome message *)
        start_header ;
        (* Start the prompt *)
        prompt program (ty_env, expr_env, event_env) )
  |> print_output ~previous_state:empty_runtime_state

let _ = runtime
