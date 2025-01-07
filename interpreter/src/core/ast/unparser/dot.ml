open Helper
(* open Common *)
(* open Monads.FilterMonad *)

open Ast
open Syntax

(* TODO *)

let rec root_graph_template =
  Printf.sprintf
    {|
   digraph %s {
      compound=true;
      ranksep=0.3;
      color=gray;

      node [shape=record, style="rounded, filled", margin="0.25,0.1", penwidth=10.0, color=gray, fillcolor=white, penwidth=1.5, fontname=Arial];
      edge [labeldistance = 0; labelangle = 0; fontsize = 14; fontname = Arial; splines = true; fontstyle = bold;];
      
      %s
   }
|}

and unparse ?(buffer = Buffer.create 100) ?(graph_name = "G") program =
  let {events; _} = program in
  let content_buffer = Buffer.create 100 in
  unparse_list ~buffer:content_buffer ~separator:"\n"
    (fun ~buffer event -> ignore (unparse_event ~buffer event))
    events ;
  let content = Buffer.contents content_buffer in
  Buffer.add_string buffer (root_graph_template graph_name content) ;
  Buffer.contents buffer

and unparse_event ?(buffer = Buffer.create 100) ?(indent = "") event =
  let {info= id, label; io; marking= _; _} = event.data in
  let unparse_info ~buffer ~indent =
    Buffer.add_string buffer indent ;
    Buffer.add_string buffer @@ Printf.sprintf "%s: %s" id.data label.data
  and unparse_io ~buffer ~indent =
    let io =
      match io.data with
      | Input ty -> "?: " ^ unparse_ty ~buffer ~indent ty.data
      | Output expr -> unparse_expr ~buffer ~indent expr
    in
    Buffer.add_string buffer indent ;
    Buffer.add_string buffer @@ Printf.sprintf "[%s]" io
  in
  Buffer.add_string buffer indent ;
  let inner = Buffer.create 100 in
  unparse_info ~buffer:inner ~indent ;
  Buffer.add_string inner " | " ;
  unparse_io ~buffer:inner ~indent ;
  Buffer.add_string buffer id.data ;
  Buffer.add_string buffer
  @@ Printf.sprintf "[ label =  \"{ %s }\" ]" (Buffer.contents inner) ;
  Buffer.contents buffer

and unparse_expr ?(buffer = Buffer.create 100) ?(indent = "") =
  Plain.unparse_expr ~buffer ~indent

and unparse_ty ?(buffer = Buffer.create 100) ?(indent = "") =
  Plain.unparse_ty ~buffer ~indent
