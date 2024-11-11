open Js_of_ocaml
open Misc.Monads
open Misc.Printing
open Templating.Syntax
open Templating.Errors

module ProgramState = struct
  open Templating.Api

  let init = ref empty_program

  let view state = view !state |> Result.get_ok

  let set state program = state := program
end

exception RuntimeError of detailed_error list

let print_output ~parse_ok ~_parse_error result =
  match result with
  | Ok res -> parse_ok res
  | Error errors -> raise (RuntimeError errors)

(* List.map (fun e -> yojson_of_detailed_error e |> Yojson.Safe.to_string )
   errors |> parse_error *)

(* Export functions to JavaScript *)
let () =
  let open Templating.Lex_and_parse in
  let open Templating.Api in
  Logger.disable () ;
  let state = ProgramState.init in
  let view () = ProgramState.view state in
  let parse str_program =
    (let lexbuf = Lexing.from_string str_program in
     parse_program lexbuf
     >>= fun program ->
     ProgramState.set state program ;
     Ok (program, "Program parsed successfully") )
    |> print_output ~parse_ok:(fun (program, _) ->
           Js.string @@ (yojson_of_program program |> Yojson.Safe.to_string) )
    (* ~parse_error:(fun errors -> Js.string @@ String.concat "\n" errors) *)
  in
  let execute event_id expr =
    (let expr_lexbuf = Lexing.from_string expr in
     parse_expression expr_lexbuf
     >>= fun expr ->
     let program = !state in
     execute ~event_id ~expr:expr.data program
     >>= fun new_program ->
     ProgramState.set state new_program ;
     Ok (new_program, "Event executed successfully") )
    |> print_output ~parse_ok:(fun (program, _) ->
           Js.string @@ (yojson_of_program program |> Yojson.Safe.to_string) )
    (* ~parse_error:(fun errors -> Js.string @@ String.concat "\n" errors) *)
  in
  Js.export "view" view ;
  Js.export "parse" (fun str_program -> parse @@ Js.to_string str_program) ;
  Js.export "execute" (fun event_id expr ->
      execute (Js.to_string event_id) (Js.to_string expr) ) ;
  ()
