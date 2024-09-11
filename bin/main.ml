open Templating
open Syntax

let _add_expr id x = BinaryOp (Identifier id, x, Add)

let _add x y = BinaryOp (IntLit x, IntLit y, Add)

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
  (* let open Instantiation in  *)
  let open Misc.Monads in
  let open Misc.Env in
  let open Runtime in
  Ok _test0 
  (* >>= instantiate  *)
  >>= execute ~event_id:"a'" ~_expr:(_add 1 2) empty_env
  |> function
    | Ok program -> view program
    | Error e -> print_endline e; view _test1

