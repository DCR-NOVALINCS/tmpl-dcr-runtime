open Syntax
open Errors
open Misc.Env

(** [execute ~event_id ~expr ?event_env ?expr_env program] executes the event
    with the given [event_id] in the [program] with the given [expr] and
    returns the updated program.
    @param event_id The id of the event
    @param expr The expression to be executed
    @param event_env The environment to be used for events
    @param expr_env The environment to be used for expressions
    @param program The program where the event is to be executed
    @return The updated program with the event executed.
*)
val execute :
  event_id:string ->
  ?expr:expr' ->
  (* ?event_env:(event env) ->
  ?expr_env:(expr env) -> *)
  program -> (program, detailed_error list) result

(** [preprocess_program ?expr_env program] preprocesses the [program] by
    evaluating all the expressions in the program and returns the environments.
    @param expr_env The environment to be used for expressions
    @param program The program to be preprocessed
    @return The environments for events and expressions.
*)
val preprocess_program :
  ?expr_env:(event env) ->
  program ->
  ((event env) * (expr env), detailed_error list) result

(** [view ?filter ?event_env ?expr_env ?should_print_events ?should_print_relations program]
    views the [program] by unpacking the events and relations in the program
    @param filter The filter to be used for evaluation
    @param event_env The environment to be used for events
    @param expr_env The environment to be used for expressions
    @param should_print_events A flag to print events
    @param should_print_relations A flag to print relations
    @param program The program to be viewed
    @return The result of the view.
*)
val view :
  ?filter:((event env) * (expr env)->
           event -> bool) ->
  (* ?event_env:(event env) ->
  ?expr_env:'a list list -> *)
  ?should_print_events:bool ->
  ?should_print_relations:bool -> program -> (string, detailed_error list) result

(** [view_debug program] views the [program] by unpacking all the events and relations in the program
    @param program The program to be viewed
    @return The result of the view.
*)
val view_debug : program -> (string, detailed_error list) result

(** [view_enabled ?event_env ?expr_env ?should_print_relations program]
    views the [program] by unpacking the events that are enabled in the program.
    @param event_env The environment to be used for events
    @param expr_env The environment to be used for expressions
    @param should_print_relations A flag to print relations
    @param program The program to be viewed
    @return The result of the view.
*)
val view_enabled :
  (* ?event_env:(event env) ->
  ?expr_env:'a list list -> *)
  ?should_print_relations:bool -> program -> (string, detailed_error list) result

(** [export_program program filename] exports the [program] to a file with the given [filename]
    @param program The program to be exported
    @param filename The filename to export the program
    @return The result of the export.
*)
val export_program : program -> string -> (unit, detailed_error list) result