open Ast
open Error
open Syntax

val type_mismatch :
     ?errors:detailed_error list
  -> ?loc:loc
  -> type_expr' list
  -> type_expr' list
  -> ('a, detailed_error list) result

val event_type_mismatch :
     ?errors:detailed_error list
  -> ?loc:loc
  -> ?available:(string * (type_expr' * event_type')) list
  -> (string * event_type' * type_expr') list
  -> (string * event_type' * type_expr') list
  -> ('a, detailed_error list) result

val missing_label :
     ?errors:detailed_error list
  -> ?available_labels:string annotated list
  -> string annotated
  -> ('a, detailed_error list) result

val property_not_found_type :
     ?errors:detailed_error list
  -> ?loc:loc
  -> string annotated
  -> type_expr'
  -> ('a, detailed_error list) result
