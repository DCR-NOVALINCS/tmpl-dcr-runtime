open Templating.Syntax
open Templating.Runtime
open Misc.Monads
(* open Misc.Env *)
open Templating.Instantiation


(*
=============================================================================
  Aux functions
=============================================================================
*)

let _add x y = BinaryOp (IntLit x, IntLit y, Add)

(*
=============================================================================
  Templates
=============================================================================
*)

(*
  tmpl g (n: Number, a: A) {
    b: B[n];
    a -->% b
  }
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

(*
tmpl i(e: [label]) {} => e
*)
let tmpl_i label = {
  id = "i";
  params = [("e", (EventTy label))];
  graph = empty_subprogram;
  export = ["e"];
}

(*
tmpl h(a: A) {
  b: B[0];
  a -->% b
} => b
*)
let tmpl_h = {
  id = "h";
  params = [("a", (EventTy "A"))];
  graph = (
    [mk_event ~id:"b" ~label:"B" (Output (IntLit 0))],
    [],
    [mk_control_relation ~from:"a" Exclude ~dest:"b"]
  );
  export = ["b"];
}

(*
=============================================================================
  Instances & Subprograms
=============================================================================
*)

let g_0_a' = mk_template_inst "g" [("n", IntLit 0); ("a", (Identifier "a'"))] ~x:[]

let i ~e ~x = mk_template_inst "i" [("e", (Identifier e))] ~x:[x]

let h ~a ~x = mk_template_inst "h" [("a", (Identifier a))] ~x:[x]

(*
=============================================================================
  Test Section
=============================================================================
*)

(*
a': A[?];
a' -->> {
  b: B[-1];
  g(0, a')
}
*)
let _test0 = {
  template_decls = [tmpl_g; tmpl_i "A"]
; events = [mk_event ~id:"a'" ~label:"A" (Input (IntTy))]
; template_insts = []
; relations = [mk_spawn_relation ~from:"a" (
    [mk_event ~id:"b" ~label:"B" (Output (PropDeref (Trigger, "value")))],
    [g_0_a'],
    []
  )]
}

(*
a': A[?];
g(0, a')
*)
let _test1 = {
  template_decls = [tmpl_g; tmpl_i "A"]
; events = [mk_event ~id:"a'" ~label:"A" (Input (UnitTy))]
; template_insts = [g_0_a']
; relations = []
}

(*
a': A[?];
i(a') => a2
*)
let _test2 = {
  template_decls = [tmpl_i "A"]
; events = [mk_event ~id:"a'" ~label:"A" (Input (UnitTy))]
; template_insts = [i ~e:"a'" ~x:"a2"]
; relations = []
} 

(*
a': A[?];
h(a') => b2
h(a') => b3
a' -->> {
  b4: B[0];
  g(0, a')
}
*)
let _test3 = {
  template_decls = [tmpl_h]
; events = [mk_event ~id:"a'" ~label:"A" (Input (UnitTy))]
; template_insts = [
  h ~a:"a'" ~x:"b2"
  ; h ~a:"a'" ~x:"b3"
]
; relations = [
  mk_spawn_relation ~from:"a'" (
    [ mk_event ~id:"b4" ~label:"B" (Output (IntLit 0))],
    [],
    [ mk_control_relation ~from:"a'" Exclude ~dest:"b2"]
  )
]
}

(*
=============================================================================
  Main Section
=============================================================================
*)  

let _ = 
  let target = _test3 in
  ( 
    Ok target
    >>= fun program ->
    preprocess_program program
    >>= fun (event_env, expr_env) ->
    instantiate ~expr_env program
    >>= fun (program, expr_env) ->
    (* execute ~event_env ~expr_env ~event_id:"a'" program
    >>= fun program ->  *)
    view ~event_env ~expr_env ~should_print_relations:true program 
  )
  |> function
  | Error e -> 
    print_endline e;
    print_endline "-----------------\n";
    print_endline @@ string_of_program target
  | _ -> ()

