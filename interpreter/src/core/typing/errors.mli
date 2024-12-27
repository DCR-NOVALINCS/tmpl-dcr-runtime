open Ast
open Error
open Syntax

val type_mismatch :
     ?errors:detailed_error list
  -> ?loc:loc
  -> type_expr' list
  -> type_expr' list
  -> ('a, detailed_error list) result
(** [type_mismatch ?errors ?loc expected actual] returns a [Result] with an
    error if the expected and actual types do not match.
    @param ?errors A list of errors to append to the result.
    @param ?loc The location of the error.
    @param expected The expected type.
    @param actual The actual type.
    @return
      A [Result] with an error if the expected and actual types do not match. *)

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

val missing_exported_event_types :
     ?errors:detailed_error list
  -> expected:event_id list
  -> event_id list
  -> ('a, detailed_error list) result
