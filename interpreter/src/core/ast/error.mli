type detailed_error =
  {location: Syntax.loc; message: string; hint: string option}

val detailed_error_of_yojson : Yojson.Safe.t -> detailed_error

val yojson_of_detailed_error : detailed_error -> Yojson.Safe.t

val todo : ?loc:Syntax.loc -> string -> ('a, detailed_error list) result

val fixme : ?loc:Syntax.loc -> string -> ('a, detailed_error list) result

val something_went_wrong :
  ?loc:Syntax.loc -> string -> ('a, detailed_error list) result

val should_not_happen :
     ?errors:detailed_error list
  -> ?loc:Syntax.loc
  -> ?module_path:string
  -> ?line:string
  -> string
  -> ('a, detailed_error list) result

val event_not_found :
     ?errors:detailed_error list
  -> ?loc:Syntax.loc
  -> string
  -> ('a, detailed_error list) result

val events_not_found :
     ?errors:detailed_error list
  -> ?loc:Syntax.loc
  -> string Syntax.annotated list
  -> ('a, detailed_error list) result

val id_not_found :
     ?errors:detailed_error list
  -> string Syntax.annotated
  -> ('a, detailed_error list) result

val tmpl_not_found :
     ?errors:detailed_error list
  -> ?available:string list
  -> string Syntax.annotated
  -> ('a, detailed_error list) result

val duplicate_tmpl :
     ?errors:detailed_error list
  -> string Syntax.annotated
  -> ('a, detailed_error list) result

val duplicate_event :
     ?errors:detailed_error list
  -> string Syntax.annotated
  -> ('a, detailed_error list) result

val value_from_input_event :
     ?errors:detailed_error list
  -> Syntax.event' Syntax.annotated
  -> ('a, detailed_error list) result

val get_line_content : string -> int -> string * int

val extract_location_info : Syntax.loc -> string option * int * int * int

val error_pointer : char

val pretty_string_error : detailed_error -> string

val pretty_string_errors : detailed_error list -> string

val print_error : detailed_error -> unit

val print_errors : detailed_error list -> unit
