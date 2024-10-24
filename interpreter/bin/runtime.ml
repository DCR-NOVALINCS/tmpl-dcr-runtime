open Js_of_ocaml
open Templating.Syntax

module ProgramState = struct
  open Templating.Api

  let init = ref empty_program

  let view state =
    view !state
    |> Result.get_ok

  let set state program =
    state := program
end

(* Export functions to JavaScript *)
let () =
  let open Templating.Lex_and_parse in 
  let open Templating.Api in
  
  let state = ProgramState.init in
  let view () = ProgramState.view state in
  let parse str_program =
    let lexbuf = Lexing.from_string str_program in
    let program = parse_program lexbuf |> Result.get_ok in
    ProgramState.set state program;
    Js.string "Parsed successfully"
  in
  let execute event_id expr = 
    let expr_lexbuf = Lexing.from_string expr in
    let expr = parse_expression expr_lexbuf |> Result.get_ok in
    let program = !state in
    let new_program = execute ~event_id ~expr:expr.data program |> Result.get_ok in
    ProgramState.set state new_program;
    Js.string "Executed successfully"
  in
  Js.export "view" view;
  Js.export "parse" (fun str_program -> parse @@ Js.to_string str_program);
  Js.export "execute" (fun event_id expr -> execute (Js.to_string event_id) (Js.to_string expr));
  ()
