open Templating.Syntax

(*
=============================================================================
  Aux functions
=============================================================================
*)

let _add x y = BinaryOp (IntLit x, IntLit y, Add)

(*
=============================================================================
  Test Section
=============================================================================
*)

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

(* Identity template *)
let tmpl_i label = {
  id = "i";
  params = [("e", (EventTy label))];
  graph = empty_subprogram;
  export = ["e"];
}

let _test0 = {
  template_decls = [tmpl_g; tmpl_i "A"]
; events = [mk_event ~id:"a'" ~label:"A" (Input (IntTy))]
; template_insts = []
; relations = [mk_spawn_relation ~from:"a" g_a']
}

let _test1 = {
  template_decls = [tmpl_g; tmpl_i "A"]
; events = [mk_event ~id:"a'" ~label:"A" (Input (UnitTy))]
; template_insts = [g_0_a']
; relations = []
}

let _test2 = {
  template_decls = [tmpl_g]
; events = [
    mk_event ~id:"a" ~label:"A" (Input (UnitTy))
  ; mk_event ~id:"b" ~label:"B" (Output (IntLit 0))
  ; mk_event ~id:"c" ~label:"C" (Output (StringLit "Hello world!"))
]
; template_insts = [g_0_a']
; relations = [
    mk_control_relation ~from:"a" Condition ~dest:"b"
    ; mk_control_relation ~from:"b" Exclude ~dest:"a"
]
}

let flow0 target = 
  let open Templating.Runtime in
  let open Misc.Monads in
  Ok target
  >>= execute ~event_id:"a" ?expr:(Some (IntLit 1))
  >>= execute ~event_id:"b"

(*
=============================================================================
  Main Section
=============================================================================
*)

let _ = 
  (* let open Instantiation in  *)
  let open Templating.Runtime in
  let open Misc.Monads in
  let target = _test2 in
  flow0 target
  >>= view
  |> function
    | Ok result -> print_endline result 
    | Error e -> 
      print_endline e;
      view_enabled target |> function
        | Ok result -> print_endline result
        | Error e -> print_endline e 

