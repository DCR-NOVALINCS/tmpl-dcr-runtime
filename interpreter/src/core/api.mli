open Ast
open Syntax
open Error
open Common
open Env
open Typing
open Helper

val initialize :
     program
  -> ( program
       * ( type_expr' env
         * event_type_value Hashtbl.Make(String).t
         * expr' annotated env
         * event' annotated env )
     , detailed_error list )
     result
(** [initialize program] initializes the [program] by ensuring that:
    - All event expressions are well-typed according to the [event_env].
    - Template instance expressions are well-typed according to the [event_env],
      [expr_env] and template definitions.
    - All relations find their corresponding events in the [event_env].

    @param program The program to initialize.
    @return
      A result containing the initialized program if the program is well-typed,
      or a list of errors if the program is not well-typed. *)

val execute :
     event_id:string
  -> ?expr:expr'
  -> ?ty_env:type_expr' env
  -> ?label_types:event_type_value Hashtbl.Make(String).t
  -> ?expr_env:expr env
  -> ?event_env:event env
  -> program
  -> ( program
       * event' annotated env
       * expr' annotated env
       * type_expr' env
       * event_type_value Hashtbl.Make(String).t
     , detailed_error list )
     result
(** [execute ~event_id ?expr ?ty_env ?label_types ?expr_env ?event_env program]
    executes the event with the given [event_id] and propagate its effects in
    the [program]. This function may return a error if the program does not meet
    the following conditions:
    - The event is not found in the [event_env].
    - The event is not enabled.
    - The expression is not well-typed.
    - The value of the marking is not the same as the received type.

    @param event_id The id of the event to execute.
    @param expr The expression to execute the event with.
    @param ty_env The environment of types to typecheck against.
    @param label_types
      The environment of event's label types to typecheck against.
    @param expr_env The environment of expressions to typecheck against.
    @param event_env The environment of events to typecheck against.
    @param program The program to execute.
    @return
      A result containing the executed program, event environment, expression
      environment, type environment and event type environment if the program is
      well-typed, or a list of errors. *)

val parse_program_from_file : string -> (program, detailed_error list) result
(** [parse_program_from_file filename] parses the program from the given
    [filename]. This function may return a error if the file does not exist.

    @param filename The name of the file to parse the program from.
    @return A result containing the parsed program, or a list of errors. *)

val parse_expression_from_string :
  string list -> (expr' annotated, detailed_error list) result
(** [parse_expression_from_string expr_tokens] parses the expression from the
    given [expr_tokens]. This function may return a error if the expression is
    not well-formed.

    @param expr_tokens The tokens of the expression to parse.
    @return A result containing the parsed expression, or a list of errors. *)

val unparse_program_tdcr :
     ?should_print_value:bool
  -> ?should_print_executed_marking:bool
  -> program
  -> (string, 'a) result
(** [unparse_program_tdcr ?should_print_value ?should_print_executed_marking
     program] unparses the [program] in string format, based on optional flags.

    @param should_print_value
      A flag to indicate whether to print the value of the marking.
    @param should_print_executed_marking
      A flag to indicate whether to print the executed marking.
    @param program The program to unparse.
    @return A result containing the unparsed program, or a list of errors. *)

val unparse_program_json : program -> (string, 'a) result
(** [unparse_program_json program] unparses the [program] in JSON format.

    @param program The program to unparse.
    @return A result containing the unparsed program, or a list of errors. *)

val unparse_program_dot : program -> (string, 'a) result
(** [unparse_program_dot program] unparses the [program] in DOT format.

    @param program The program to unparse.
    @return A result containing the unparsed program, or a list of errors. *)

val view :
     ?filter:(event -> event env * expr env -> event option)
  -> ?should_print_template_decls:bool
  -> ?should_print_events:bool
  -> ?should_print_value:bool
  -> ?should_print_relations:bool
  -> ?expr_env:expr env
  -> ?event_env:event env
  -> program
  -> (string, 'a) result
(** [view ?filter ?should_print_template_decls ?should_print_events
     ?should_print_value ?should_print_relations ?expr_env ?event_env program]
    views the [program] in string format, based on optional flags. Is the same
    as {i unparse_program_tdcr} but with more options.

    @param filter A function to filter the events to view.
    @param should_print_template_decls
      A flag to indicate whether to print the template declarations.
    @param should_print_events A flag to indicate whether to print the events.
    @param should_print_value
      A flag to indicate whether to print the value of the marking.
    @param should_print_relations
      A flag to indicate whether to print the relations.
    @param expr_env The environment of expressions to typecheck against.
    @param event_env The environment of events to typecheck against.
    @param program The program to view.
    @return A result containing the viewed program, or a list of errors. *)

val view_debug : program -> (string, 'a) result
(** [view_debug program] views the [program] in string format, with debug
    information.

    @param program The program to view.
    @return A result containing the viewed program, or a list of errors. *)

val view_enabled :
     ?should_print_template_decls:bool
  -> ?should_print_value:bool
  -> ?should_print_relations:bool
  -> ?expr_env:expr env
  -> ?event_env:event env
  -> program
  -> (string, 'a) result
(** [view_enabled ?should_print_template_decls ?should_print_value
     ?should_print_relations ?expr_env ?event_env program] views the enabled
    events in the [program] in string format, based on optional flags.

    @param should_print_template_decls
      A flag to indicate whether to print the template declarations.
    @param should_print_value
      A flag to indicate whether to print the value of the marking.
    @param should_print_relations
      A flag to indicate whether to print the relations.
    @param expr_env The environment of expressions to typecheck against.
    @param event_env The environment of events to typecheck against.
    @param program The program to view.
    @return A result containing the viewed program, or a list of errors. *)

val view_disabled :
     ?should_print_template_decls:bool
  -> ?should_print_value:bool
  -> ?should_print_relations:bool
  -> ?expr_env:expr env
  -> ?event_env:event env
  -> program
  -> (string, 'a) result
(** [view_disabled ?should_print_template_decls ?should_print_value
     ?should_print_relations ?expr_env ?event_env program] views the disabled
    events in the [program] in string format, based on optional flags.

    @param should_print_template_decls
      A flag to indicate whether to print the template declarations.
    @param should_print_value
      A flag to indicate whether to print the value of the marking.
    @param should_print_relations
      A flag to indicate whether to print the relations.
    @param expr_env The environment of expressions to typecheck against.
    @param event_env The environment of events to typecheck against.
    @param program The program to view.
    @return A result containing the viewed program, or a list of errors. *)
