open Templating.Syntax
open Templating.Runtime
open Misc.Monads
open Templating.Instantiation


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
    ; mk_control_relation ~from:"a" Response ~dest:"c"
]
}

let _test3 = {
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
    ; mk_control_relation ~from:"a" Response ~dest:"c"
    ; mk_spawn_relation ~from:"a" (
      [mk_event ~id:"b2" ~label:"B" (Output (IntLit 0))],
      [],
      [mk_control_relation ~from:"a" Exclude ~dest:"b2"]
    )
]
}

let _test4 = {
  template_decls = [tmpl_g]
; events = [
    mk_event ~id:"a'" ~label:"A" (Input (UnitTy))
]
; template_insts = [
  (* g_0_a' *)
]
; relations = [
    mk_spawn_relation ~from:"a'" g_a'
]
}

let _trace0 event_env expr_env target = 
  Ok target
  >>= execute ~event_env ~expr_env ~event_id:"a" ?expr:(Some (IntLit 1)) 
  >>= execute ~event_env ~expr_env ~event_id:"a" ?expr:(Some (IntLit 1)) 
  (* >>= execute ~event_id:"b" *)

let _trace1 event_env expr_env target = 
  Ok target
  >>= execute ~event_env ~expr_env ~event_id:"a'" ?expr:(Some (BinaryOp (IntLit 1, IntLit 2, Add))) 
  (* >>= execute ~event_id:"b" *)

(*
=============================================================================
  Main Section
=============================================================================
*)  

let execute_traces event_env expr_env traces program = 
  fold_left_result 
    (fun program trace -> 
      trace event_env expr_env program)
    program traces

let _ = 
  let target = _test4 in
  ( preprocess_program target
  >>= fun (event_env, expr_env) ->
  instantiate ~expr_env target
  >>= fun (program, expr_env) -> 
  execute_traces event_env expr_env [_trace1] program
  >>= fun program -> 
  view ~event_env ~expr_env program )
  |> function
  | Error e -> 
    print_endline e;
    print_endline "-----------------";
    (string_of_program target) |> print_endline 
  | _ -> ()

