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
  tmpl g (n: Number, e: A) {
    b: B[n];
    e -->* b
  }
*)
let tmpl_g = {
  id = "g";
  params = [("n", IntTy); ("a", (EventTy "A"))];
  graph = (
    [mk_event ~id:"b" ~label:"B" (Output (Identifier "n"))],
    [],
    [mk_control_relation ~from:"a" Condition ~dest:"b"]
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
tmpl j() {
  e: E[?]
  k(e) => e2
} => e2
*)
let tmpl_j = {
  id = "j";
  params = [];
  graph = (
    [mk_event ~id:"e" ~label:"E" (Input (UnitTy))],
    [mk_template_inst "k" [("e", (Identifier "e"))] ~x:["e2"]],
    []
  );
  export = ["e"];
}

(*
tmpl k(e: E[?]) E {
  j() => e
} => e
*)
let tmpl_k = {
  id = "k";
  params = [("e", (EventTy "E"))];
  graph = (
    [],
    [mk_template_inst "j" [] ~x:["e"]],
    []
  );
  export = ["e"];
}

(*
tmpl fac(n: Number) NumberHolder {
    fac_aux(1, 1, n) => r
} => r
*)
let tmpl_fac = {
  id = "fac";
  params = [("n", IntTy)];
  graph = (
    [],
    [mk_template_inst "fac_aux" [("acc", IntLit 1); ("i", IntLit 1); ("n", (Identifier "n"))] ~x:["r"]],
    []
  );
  export = ["r"]
}

(*
tmpl fac_aux(acc: Number, i: Number, n: Number) NumberHolder {
    r: NumberHolder[acc] -- when i >= n
    ;
    fac_aux(acc = i * acc, i = i + 1, n = n) => r -- when i < n
} => r
*)
let tmpl_fac_aux = {
  id = "fac_aux";
  params = [("acc", IntTy); ("i", IntTy); ("n", IntTy)];
  graph = (
    [mk_event ~id:"r" ~label:"NumberHolder" (Output (Identifier "acc")) 
    ~annotations:[When (BinaryOp (Identifier "i", Identifier "n", GreaterOrEqual))]],
    [mk_template_inst "fac_aux" 
      [ ("acc", BinaryOp (Identifier "i", Identifier "acc", Mult))
      ; ("i", BinaryOp (Identifier "i", IntLit 1, Add))
      ; ("n", (Identifier "n"))
      ]
      ~x:["r"] ~annotations:[When (BinaryOp (Identifier "i", Identifier "n", LessThan))]
    ],
    []
  );
  export = ["r"]
}

(*
=============================================================================
  Instances & Subprograms
=============================================================================
*)

let f ~num ~x = mk_template_inst "f" [("num", (IntLit num))] ~x:[x]

let g ~n ~a = mk_template_inst "g" [("n", (IntLit n)); ("a", (Identifier a))] ~x:[]

let h ~a ~x = mk_template_inst "h" [("a", (Identifier a))] ~x:[x]

let i ~e ~x = mk_template_inst "i" [("e", (Identifier e))] ~x:[x]

let j ~x = mk_template_inst "j" [] ~x:[x]

let k ~e ~x = mk_template_inst "k" [("e", (Identifier e))] ~x:[x]

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
    []
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

(*
j() => e
*)
let _test8 = {
  template_decls = [tmpl_j; tmpl_k]
; events = []
; template_insts = [
  mk_template_inst "j" [] ~x:["e"]
]
; relations = []
}

(*
a: A[?] -- when 0 > 1
b: B[?]
;
a -->* b
b -->% a
*)
let _test9 = {
  template_decls = []
; events = [
  mk_event ~id:"a" ~label:"A" (Input (UnitTy)) ~annotations:[When (BinaryOp (IntLit 0, IntLit 1, GreaterThan))];
  mk_event ~id:"b" ~label:"B" (Input (UnitTy))
]
; template_insts = []
; relations = [
  mk_control_relation ~from:"a" Condition ~dest:"b";
  mk_control_relation ~from:"b" Exclude ~dest:"a"
]
}

(*
a': A[?: Number]
;
a' -->> {
  g(0, a') -- when @trigger.value > 0
}
*)
let _test10 = {
  template_decls = [tmpl_g]
; events = [mk_event ~id:"a'" ~label:"A" (Input (IntTy))]
; template_insts = []
; relations = [
  mk_spawn_relation ~from:"a'" (
    [],
    [mk_template_inst "g" [("n", IntLit 0); ("a", (Identifier "a'"))] ~x:[] 
    ~annotations:[When (BinaryOp (PropDeref(Trigger, "value"), IntLit 0, GreaterThan))]],
    []
  )
]
}

(*
a: A[?: Number]
;
a -->> {
  fac(@trigger.value) => result
} 
*)
let _test11 = {
  template_decls = [tmpl_fac; tmpl_fac_aux]
; events = [mk_event ~id:"a" ~label:"A" (Input (IntTy))]
; template_insts = []
; relations = [
  mk_spawn_relation ~from:"a" (
    [],
    [
      mk_template_inst "fac" [("n", PropDeref (Trigger, "value"))] ~x:["result"]
    ],
    []
  ) 
]
}

(*
a: A[?]
;
a -->> {
  fac(i) => result -- foreach i in [0, 1, 2]
} 
*)
let _test12 = {
  template_decls = [tmpl_fac; tmpl_fac_aux]
; events = [mk_event ~id:"a" ~label:"A" (Input (UnitTy))]
; template_insts = []
; relations = [
  mk_spawn_relation ~from:"a" (
    [],
    [
      mk_template_inst "fac" [("n", Identifier "i")] ~x:["result"]
      ~annotations:[ Foreach ("i", List [IntLit 0; IntLit 1; IntLit 2]) ]  
    ],
    []
  ) 
]
}

(*
a: A[?]
b: B[i] -- foreach i in [0, 1, 2]
;
*)
let _test13 = {
  template_decls = []
; events = [
  mk_event ~id:"a" ~label:"A" (Input (UnitTy))
  ; mk_event ~id:"b" ~label:"B" (Output (Identifier "i")) 
  ~annotations: [ Foreach ("i", List([IntLit 0; IntLit 1; IntLit 2])) ]
] 
; template_insts = []
; relations = []
}

(*
=============================================================================
  Robot Watering Plant Example
=============================================================================
*)

let robot_tmpl = {
  id = "robot";
  params = [("id", StringTy)];
  graph = (
    [
      mk_event ~id:"r" ~label:"Robot" (Output (Record [("id", Identifier "id")]));
      mk_event ~id:"m" ~label:"move" (Input (RecordTy [("x", IntTy); ("y", IntTy)]));
      mk_event ~id:"cf" ~label:"collectFruits" (Input (EventTy "Plant"));
      mk_event ~id:"w" ~label:"watering" (Input (EventTy "Plant"));
      mk_event ~id:"t" ~label:"trim" (Input (EventTy "Plant"));
    ],
    [],
    [
      mk_control_relation ~from:"m" Condition ~dest:"cf";
      mk_control_relation ~from:"m" Condition ~dest:"w";
      mk_control_relation ~from:"m" Condition ~dest:"t";
      mk_control_relation ~from:"cf" Milestone ~dest:"wp";
      mk_control_relation ~from:"cf" Milestone ~dest:"t";
      mk_control_relation ~from:"w" Milestone ~dest:"cf";
      mk_control_relation ~from:"w" Milestone ~dest:"t";
      mk_control_relation ~from:"t" Milestone ~dest:"w";
      mk_control_relation ~from:"t" Milestone ~dest:"cf";
      ]
  );
  export = ["w"; "cf"; "t"]
}

let robot ~id ~x ~annotations = mk_template_inst "robot" [("id", id)] ~x ~annotations

let plant_tmpl = {
  id = "plant";
  params = [
    ("id", StringTy);
    ("position", RecordTy [("x", IntTy); ("y", IntTy)]);
    ("plantType", RecordTy [("typename", StringTy)]);
    ("wp", EventTy "watering");
  ];
  graph = (
    [
      mk_event ~id:"p" ~label:"Plant" (Output (Record [("id", Identifier "id"); ("plantType", Identifier "plantType"); ("position", Identifier "position")]));
      mk_event ~id:"nw" ~label:"needWatering" (Input (UnitTy));
    ],
    [],
    [
      mk_control_relation ~from:"nw" Exclude ~dest:"nw";
      mk_control_relation ~from:"nw" Response ~dest:"wp";
      mk_control_relation ~from:"nw" Include ~dest:"nw";
    ]
  );
  export = []
}

let plant ~id ~position ~plantType ~wp ~annotations = mk_template_inst "plant" [
  ("id", id);
  ("position", position);
  ("plantType", plantType);
  ("wp", wp)
] ~x:[] ~annotations:annotations

let _robot_plant_watering_test = {
  template_decls = [robot_tmpl; plant_tmpl]
; events = [
  mk_event ~id:"ar" ~label:"addRobot" (Input (RecordTy [("id", StringTy); ("count", IntTy)]));
  mk_event ~id:"ap" ~label:"addPlant" (Input (RecordTy [("id", StringTy); ("count", IntTy); ("typename", StringTy); ("position", RecordTy [("x", IntTy); ("y", IntTy)])]));
]
; template_insts = []
; relations = [
  mk_spawn_relation ~from:"ar" (
    [],
    [
      robot ~id:(PropDeref(PropDeref (Trigger, "value"), "id")) ~x:["w"; "cf"; "t"]
      ~annotations: [
        Foreach ("i", List (List.init 3 (fun i -> IntLit i)));
      ]
    ],
    [
      mk_spawn_relation ~from:"ap" (
        [],
        [plant 
          ~id:(PropDeref(PropDeref (Trigger, "value"), "id"))
          ~position:(PropDeref(PropDeref (Trigger, "value"), "position"))
          ~plantType:(Identifier ("plantType"))
          ~wp:(Identifier "w")
          ~annotations: [
            When (BinaryOp (Identifier "plantType", PropDeref (PropDeref (Trigger, "value"), "typename"), Eq))
            ; Foreach ("i", List (List.init 4 (fun i -> IntLit i)));
          ]
        ],
        []
      ) ~annotations: [Foreach ("plantType", List [
        Record [("typename", StringLit "tree")]
        ; Record [("typename", StringLit "bush")]
        ; Record [("typename", StringLit "flower")]
        ; Record [("typename", StringLit "cactus")]
        ])]
    ]
  );
  
]
}