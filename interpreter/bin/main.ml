open Templating
open Api
open Errors
open Syntax
open Instantiation
open Lex_and_parse
open Typechecking

(* open Unparser *)
open Program_helper
open Misc

(* open Env *)
open Monads.ResultMonad
open Printing

(* open Misc.Env *)

(* =============================================================================
   Aux functions
   ============================================================================= *)

let rec start_header =
  CPrinter.cprint "To get started, type " ;
  CPrinter.cprint ~color:Green "help" ;
  CPrinter.cprint " or " ;
  CPrinter.cprint ~color:Green "h" ;
  CPrinter.cprintln " to see the available commands.\n"

and get_file_extension filename =
  let parts = String.split_on_char '.' filename in
  List.nth parts (List.length parts - 1)

and input_file_extension = "tdcr"

and input_file filename =
  if not (Sys.file_exists filename) then file_not_exists filename
  else
    get_file_extension filename
    |> function
    | ext when ext = input_file_extension -> return ()
    | got -> invalid_file_extension ~supported:input_file_extension ~got ()

(* and write_to_file content filename =
   let oc = open_out filename in
   Printf.fprintf oc "%s\n" content ;
   close_out oc *)

and sanitize_input input =
  input |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") |> return

(* =============================================================================
   Commands Section
   ============================================================================= *)

let print_output ?(previous_state = empty_runtime_state) = function
  | Ok runtime_state ->
      let {output; _} = runtime_state in
      CPrinter.cprintln output ; return runtime_state
  | Error errors ->
      (* Logger.error "Errors found" ; *)
      print_errors errors ; return previous_state

(* let parse_expression expr_string =
   let expr = expr_string |> String.concat " " in
   let expr_lexbuf = Lexing.from_string expr in
   Logger.debug @@ "Parsing expression " ^ expr ;
   parse_expression expr_lexbuf *)

(* let parse filename =
   if Sys.file_exists filename then (
     Logger.debug @@ "Reading file: " ^ CString.colorize ~color:Yellow filename ;
     let lexbuf = Lexing.from_channel (open_in filename) in
     let prog = parse_program ~filename lexbuf in
     prog )
   else file_not_exists filename *)

(* let get_program =
   if Array.length Sys.argv < 2 then return empty_program
   else
     let filename = Sys.argv.(1) in
     input_file filename >>= fun _ -> parse filename *)

(* =============================================================================
   Command Management Section
   ============================================================================= *)

open Cmdliner

(* =============================================================================
   Main Section
   ============================================================================= *)

(* let read_command tokens program (ty_env, expr_env, event_env) =
     Logger.debug @@ "Command: " ^ String.concat " | " tokens ;
     match tokens with
     | ["exit"] | ["q"] -> exit 0
     | ["help"] | ["h"] ->
         (* return (program, help_message cmds) *)
         return
           (mk_runtime_state ~ty_env ~expr_env ~event_env
              ~output:(help_message cmds) program )
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
         List.iter (write_to_file unparsed_program) filenames ;
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
         invalid_command ~nearest ~distance tokens

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
     Logger.set_logger_level Debug ;
     let initialize_program =
       (* Get & Parse the initial input *)
       get_program
       >>= fun program ->
       Logger.info "Program parsed successfully" ;
       (* Preprocess program *)
       preprocess_program program
       >>= fun (event_env, expr_env, program) ->
       (* Typecheck program *)
       typecheck ~event_env program
       >>= fun (ty_env, event_env) ->
       Logger.info "Program typechecked successfully" ;
       (* Instantiate initial templates *)
       instantiate ~expr_env ~event_env program
       >>| fun (program, event_env, expr_env) ->
       Logger.info "Program instantiated successfully" ;
       (ty_env, expr_env, event_env, program)
     in
     initialize_program
     >>= (fun (ty_env, expr_env, event_env, program) ->
           (* Display welcome message *)
           start_header ;
           (* Start the prompt *)
           prompt program (ty_env, expr_env, event_env) )
     |> print_output ~previous_state:empty_runtime_state

   let _ = runtime *)

(* =============================================================================
   REPL Section
   ============================================================================= *)

let rec interpret_command tokens runtime_state =
  match tokens with
  | [] -> return runtime_state
  | cmd_name :: _ -> (
    match List.assoc_opt cmd_name cmds with
    | None ->
        let open Misc.Bktree in
        let nearest, distance =
          nearest_neighbor levenshtein_distance cmds_bbk_tree cmd_name
        in
        invalid_command ~nearest ~distance tokens
    | Some cmd -> (
        let argv = Array.of_list tokens in
        match Cmd.eval_value ~argv cmd with
        | Ok _ -> return runtime_state
        | Error _ ->
            CPrinter.cprintln "Command failed" ;
            return runtime_state ) )

and prompt runtime_state =
  start_header ;
  CPrinter.cprint ~color:BrightGreen "> " ;
  flush stdout ;
  (* Get command from input *)
  match input_line stdin with
  | exception End_of_file -> return runtime_state
  | input ->
      sanitize_input input
      >>= fun tokens ->
      interpret_command tokens runtime_state
      |> print_output ~previous_state:runtime_state
      >>= fun state -> prompt state

let runtime filename =
  input_file filename
  >>= fun _ ->
  let lexbuf = Lexing.from_channel (open_in filename) in
  parse_program ~filename lexbuf
  >>= fun program ->
  preprocess_program program
  >>= fun (event_env, expr_env, program) ->
  typecheck ~event_env program
  >>= fun (ty_env, event_env) ->
  instantiate ~expr_env ~event_env program
  >>= fun (program, event_env, expr_env) ->
  let runtime_state = mk_runtime_state ~ty_env ~expr_env ~event_env program in
  prompt runtime_state

let runtime_cmd =
  let info =
    let doc =
      "An implementation of a interpreter for Templates in DCR Graphs"
    in
    Cmd.info ~doc ~version:"0.1" "tmpl_dcr"
  and input_filename =
    let doc = "The input file to be processed" in
    Arg.(required & pos 0 (some string) None & info [] ~doc)
  in
  let input_program_main = Term.(const runtime $ input_filename) in
  Cmd.v info input_program_main

let _ = match Cmd.eval_value runtime_cmd with Ok _ -> () | Error _ -> ()
