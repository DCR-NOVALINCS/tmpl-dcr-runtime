open Ast.Syntax

let event_as_ty event =
  let {io= _; info= _, label; _} = event.data in
  (* let ty =
       match io.data with
       | Input ty -> ty.data
       | Output expr -> (
         match !(expr.ty) with None -> failwith "Type not found" | Some ty -> ty )
     in *)
  (* RecordTy [(annotate "value", annotate ty)] *)
  EventTy label
