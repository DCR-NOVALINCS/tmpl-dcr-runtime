open Templating
open Syntax

let test0 = {
  template_decls = []
; events = [
  mk_event ~id:"a" ~label:"A" (Input (UnitTy));
  mk_event ~id:"b" ~label:"B" (Output (IntLit 0));
]
; template_insts = []
; relations = [
  mk_control_relation ~from:"a" Exclude ~dest:"b";
  (* mk_spawn_relation ~from:"a" ; *)
]
}

let _ = 
  let open Instantiation in 
  let program = test0 in
  instantiate program 
  (* Ok program *)
  |> function
    | Ok program -> print_endline @@ string_of_program program
    | Error e -> print_endline e

