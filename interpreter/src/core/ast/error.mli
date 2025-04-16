(** {1 Error Handling Module}

    This module provides functions and types for handling errors in the
    interpreter. It includes detailed error reporting, including location,
    message, and optional hints. *)

(** Represents a detailed error with location, message, and an optional hint. *)
type detailed_error =
  {location: Syntax.loc; message: string; hint: string option}

val detailed_error_of_yojson : Yojson.Safe.t -> detailed_error
(** Converts a JSON representation to a [detailed_error]. *)

val yojson_of_detailed_error : detailed_error -> Yojson.Safe.t
(** Converts a [detailed_error] to its JSON representation. *)

val todo : ?loc:Syntax.loc -> string -> ('a, detailed_error list) result
(** [todo ?loc message] creates a result indicating a piece of code that is not
    yet implemented. The [loc] parameter specifies the location of the code, if
    available. *)

val fixme : ?loc:Syntax.loc -> string -> ('a, detailed_error list) result
(** [fixme ?loc message] creates a result indicating a piece of code that needs
    to be fixed. The [loc] parameter specifies the location of the code, if
    available. *)

val something_went_wrong :
  ?loc:Syntax.loc -> string -> ('a, detailed_error list) result
(** [something_went_wrong ?loc message] creates a result indicating an
    unexpected issue in the code. The [loc] parameter specifies the location of
    the issue, if available. *)

val should_not_happen :
     ?errors:detailed_error list
  -> ?loc:Syntax.loc
  -> ?module_path:string
  -> ?line:string
  -> string
  -> ('a, detailed_error list) result
(** [should_not_happen ?errors ?loc ?module_path ?line message] creates a result
    for situations that should not occur. Additional context such as errors,
    location, module path, and line can be provided. *)

val event_not_found :
     ?errors:detailed_error list
  -> ?loc:Syntax.loc
  -> string
  -> ('a, detailed_error list) result
(** [event_not_found ?errors ?loc message] creates a result indicating that a
    specific event could not be found. Additional errors and location can be
    provided. *)

val events_not_found :
     ?errors:detailed_error list
  -> ?loc:Syntax.loc
  -> string Syntax.annotated list
  -> ('a, detailed_error list) result
(** [events_not_found ?errors ?loc events] creates a result indicating that
    multiple events could not be found. Additional errors and location can be
    provided. *)

val id_not_found :
     ?errors:detailed_error list
  -> string Syntax.annotated
  -> ('a, detailed_error list) result
(** [id_not_found ?errors id] creates a result indicating that a specific
    identifier could not be found. Additional errors can be provided. *)

val tmpl_not_found :
     ?errors:detailed_error list
  -> ?available:string list
  -> string Syntax.annotated
  -> ('a, detailed_error list) result
(** [tmpl_not_found ?errors ?available tmpl] creates a result indicating that a
    specific template could not be found. Additional errors and a list of
    available templates can be provided. *)

val duplicate_tmpl :
     ?errors:detailed_error list
  -> string Syntax.annotated
  -> ('a, detailed_error list) result
(** [duplicate_tmpl ?errors tmpl] creates a result indicating that a duplicate
    template was found. Additional errors can be provided. *)

val duplicate_event :
     ?errors:detailed_error list
  -> string Syntax.annotated
  -> ('a, detailed_error list) result
(** [duplicate_event ?errors event] creates a result indicating that a duplicate
    event was found. Additional errors can be provided. *)

val value_from_input_event :
     ?errors:detailed_error list
  -> Syntax.event' Syntax.annotated
  -> ('a, detailed_error list) result
(** [value_from_input_event ?errors event] creates a result indicating an issue
    with extracting a value from an input event. Additional errors can be
    provided. *)

val pretty_string_error : detailed_error -> string
(** [pretty_string_error error] converts a [detailed_error] into a
    human-readable string. *)

val pretty_string_errors : detailed_error list -> string
(** [pretty_string_errors errors] converts a list of [detailed_error] into a
    human-readable string. *)

val print_error : detailed_error -> unit
(** [print_error error] prints a [detailed_error] to the standard output. *)

val print_errors : detailed_error list -> unit
(** [print_errors errors] prints a list of [detailed_error] to the standard
    output. *)
