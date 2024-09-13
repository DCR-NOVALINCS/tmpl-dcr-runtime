open Syntax

let add x y = BinaryOp (x, y, Add)

(*
=============================================================================
  Templates
=============================================================================
*)

(*
tmpl f(num: Number) {
  a: A[?];
  g(num, a)
} => a

*)
let tmpl_f = {
  id = "f";
  params = [("num", IntTy)];
  graph = (
    [mk_event ~id:"a" ~label:"A" (Input (UnitTy))],
    [mk_template_inst "g" [("n", add (Identifier "num") (IntLit 1)); ("a", (Identifier "a"))] ~x:[]],
    (* [], *)
    []
  );
  export = ["a"]
}

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
tmpl i(e: [label]) {} => e
*)
let tmpl_i label = {
  id = "i";
  params = [("e", (EventTy label))];
  graph = empty_subprogram;
  export = ["e"];
}

(*
=============================================================================
  Instances & Subprograms
=============================================================================
*)

let g ~n ~a = mk_template_inst "g" [("n", (IntLit n)); ("a", (Identifier a))] ~x:[]

let i ~e ~x = mk_template_inst "i" [("e", (Identifier e))] ~x:[x]

let h ~a ~x = mk_template_inst "h" [("a", (Identifier a))] ~x:[x]


(*
=============================================================================
  Test Section
=============================================================================
*)

(*
a': A[?: Number];
a' -->> {
  b: B[@trigger.value];
  g(0, a')
}
*)
let _test0 = {
  template_decls = [tmpl_g; tmpl_i "A"]
; events = [mk_event ~id:"a'" ~label:"A" (Input (IntTy))]
; template_insts = []
; relations = [mk_spawn_relation ~from:"a'" (
    [mk_event ~id:"b" ~label:"B" (Output (PropDeref (Trigger, "value")))],
    [g ~n:0 ~a:"a'"],
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
; template_insts = [g ~n:0 ~a:"a'"]
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
a': A[?];
a' -->> {
  b: B[0];
  g(0, a')
}
a' -->> {
  b: B[1];
  g(1, a')
}
*)
let _test4 = {
  template_decls = [ tmpl_g ]
; events = [ mk_event ~id:"a'" ~label:"A" (Input (UnitTy))]
; template_insts = []
; relations = [
  mk_spawn_relation ~from:"a'" (
    [ mk_event ~id:"b" ~label:"B" (Output (IntLit 0))],
    [ mk_template_inst "g" [("n", IntLit 0); ("a", (Identifier "a'"))] ~x:[]],
    []
  );
  mk_spawn_relation ~from:"a'" (
    [ mk_event ~id:"b" ~label:"B" (Output (IntLit 1))],
    [ mk_template_inst "g" [("n", IntLit 1); ("a", (Identifier "a'"))] ~x:[]],
    []
  )
]
}

(*
f(5)
*)
let _test5 = {
  template_decls = [tmpl_f; tmpl_g]
; events = []
; template_insts = [
  mk_template_inst "f" [("num", IntLit 5)] ~x:["a'"]
]
; relations = []
}

(*
a: A[?];
f(5);
a -->> {
  f(2)
}
*)
let _test6 = {
  template_decls = [tmpl_f; tmpl_g]
; events = [
  mk_event ~id:"a" ~label:"A" (Input (UnitTy))
]
; template_insts = [
  mk_template_inst "f" [("num", IntLit 5)] ~x:["a'"]
]
; relations = [
  mk_spawn_relation ~from:"a" (
    [], 
    [mk_template_inst "f" [("num", IntLit 2)] ~x:["a''"]],
    []
  )
]
}

(*
TODO: Test feeding a param with a exported event
*)
(*
tmpl i(e: A) {} => e

a: A[?];
iA(a) => a'
iA(a') => a''
*)
let _test7 = {
  template_decls = [tmpl_i "A"]
; events = [
  mk_event ~id:"a" ~label:"A" (Input (UnitTy))
]
; template_insts = [
  mk_template_inst "i" [("e", (Identifier "a"))] ~x:["a'"]
  ; mk_template_inst "i" [("e", (Identifier "a'"))] ~x:["a''"]
]
; relations = []
}