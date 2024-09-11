open Templating
open Syntax

let _add_expr id x = BinaryOp (Identifier id, x, Add)

let tmpl_g = {
  id = "g";
  params = [("n", IntTy); ("a", (EventTy "A"))];
  graph = (
    [mk_event ~id:"b" ~label:"B" (Output (Identifier "n"))],
    [],
    [mk_control_relation ~from:"a" Exclude ~dest:"b"]
  );
  export = [];
}

let g_0_a' = mk_template_inst "g" [("n", IntLit 0); ("a", (Identifier "a'"))] ~x:[]

let g_a' = (
  [],
  [g_0_a'],
  []
)

(* TODO: Identity template *)

let _test0 = {
  template_decls = [tmpl_g]
; events = [mk_event ~id:"a'" ~label:"A" (Input (UnitTy))]
; template_insts = []
; relations = [mk_spawn_relation ~from:"a" g_a']
}

let _test1 = {
  template_decls = [tmpl_g]
; events = [mk_event ~id:"a'" ~label:"A" (Input (UnitTy))]
; template_insts = [g_0_a']
; relations = []
}

(*
=============================================================================
  Main Section
=============================================================================
*)

let _ = 
  let open Instantiation in 
  let program = _test1 in
  instantiate program 
  (* Ok program *)
  |> function
    | Ok program -> print_endline @@ string_of_program program
    | Error e -> print_endline e

