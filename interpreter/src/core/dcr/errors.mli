val property_not_found :
     ?errors:Ast.Error.detailed_error list
  -> string Ast.Syntax.annotated
  -> Ast.Syntax.expr' Ast.Syntax.annotated
  -> ('a, Ast.Error.detailed_error list) result
(** [property_not_found ?errors property expr] is used to indicate that a
    specific property was not found in the given context. Optionally, a list of
    detailed errors can be provided. Returns a result type with either a success
    value or a list of detailed errors.

    @param errors Optional list of detailed errors.
    @param property The annotated property name that was not found.
    @param expr The annotated expression where the property was expected.
    @return
      A result type with either a success value or a list of detailed errors. *)

val invalid_number_of_exported_events :
     ?errors:Ast.Error.detailed_error list
  -> ?loc:Ast.Syntax.loc
  -> 'a list
  -> 'b list
  -> ('c, Ast.Error.detailed_error list) result
(** [invalid_number_of_exported_events ?errors ?loc events exported_events] is
    used to indicate that the number of exported events does not match the
    expected number. Optionally, a list of detailed errors and a location can be
    provided. Returns a result type with either a success value or a list of
    detailed errors.

    @param errors Optional list of detailed errors.
    @param loc Optional location information.
    @param events The list of events.
    @param exported_events The list of exported events.
    @return
      A result type with either a success value or a list of detailed errors. *)

val event_not_enabled :
     ?errors:Ast.Error.detailed_error list
  -> Ast.Syntax.event' Ast.Syntax.annotated
  -> ('a, Ast.Error.detailed_error list) result
(** [event_not_enabled ?errors event] is used to indicate that a specific event
    is not enabled. Optionally, a list of detailed errors can be provided.
    Returns a result type with either a success value or a list of detailed
    errors.

    @param errors Optional list of detailed errors.
    @param event The annotated event that is not enabled.
    @return
      A result type with either a success value or a list of detailed errors. *)

val param_not_found :
     ?errors:Ast.Error.detailed_error list
  -> string Ast.Syntax.annotated
  -> ('a, Ast.Error.detailed_error list) result
(** [param_not_found ?errors param] is used to indicate that a specific
    parameter was not found. Optionally, a list of detailed errors can be
    provided. Returns a result type with either a success value or a list of
    detailed errors.

    @param errors Optional list of detailed errors.
    @param param The annotated parameter name that was not found.
    @return
      A result type with either a success value or a list of detailed errors. *)

val invalid_expr :
     ?errors:Ast.Error.detailed_error list
  -> 'a Ast.Syntax.annotated
  -> ('b, Ast.Error.detailed_error list) result
(** [invalid_expr ?errors expr] is used to indicate that a specific expression
    is invalid. Optionally, a list of detailed errors can be provided. Returns a
    result type with either a success value or a list of detailed errors.

    @param errors Optional list of detailed errors.
    @param expr The annotated expression that is invalid.
    @return
      A result type with either a success value or a list of detailed errors. *)

val unknown_error :
     ?errors:Ast.Error.detailed_error list
  -> string
  -> ('a, Ast.Error.detailed_error list) result
(** [unknown_error ?errors message] is used to indicate an unknown error with a
    specific message. Optionally, a list of detailed errors can be provided.
    Returns a result type with either a success value or a list of detailed
    errors.

    @param errors Optional list of detailed errors.
    @param message The error message describing the unknown error.
    @return
      A result type with either a success value or a list of detailed errors. *)
