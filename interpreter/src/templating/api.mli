open Syntax
open Errors
open Misc.Env

val execute :
     event_id:string
  -> ?expr:expr'
  -> (* ?event_env:(event env) -> ?expr_env:(expr env) -> *)
     program
  -> (program, detailed_error list) result
(** [execute ~event_id ~expr ?event_env ?expr_env program] executes the event
    with the given [event_id] in the [program] with the given [expr] and
    returns the updated program.
    @param event_id The id of the event
    @param expr The expression to be executed
    @param event_env The environment to be used for events
    @param expr_env The environment to be used for expressions
    @param program The program where the event is to be executed
    @return The updated program with the event executed. *)

val preprocess_program :
     ?expr_env:expr env
  -> program
  -> (event env * expr env * program, detailed_error list) result
(** [preprocess_program ?expr_env program] preprocesses the [program] by
    evaluating all the expressions in the program and returns the environments.
    @param expr_env The environment to be used for expressions
    @param program The program to be preprocessed
    @return The environments for events and expressions. *)

val view :
     ?filter:(event env * expr env -> event -> bool)
  -> (* ?event_env:(event env) -> ?expr_env:'a list list -> *)
     ?should_print_events:bool
  -> ?should_print_relations:bool
  -> program
  -> (string, detailed_error list) result
(** [view ?filter ?event_env ?expr_env ?should_print_events ?should_print_relations program]
    views the [program] by unpacking the events and relations in the program
    @param filter The filter to be used for evaluation
    @param event_env The environment to be used for events
    @param expr_env The environment to be used for expressions
    @param should_print_events A flag to print events
    @param should_print_relations A flag to print relations
    @param program The program to be viewed
    @return The result of the view. *)

val view_debug : program -> (string, detailed_error list) result
(** [view_debug program] views the [program] by unpacking all the events and relations in the program
    @param program The program to be viewed
    @return The result of the view. *)

val view_enabled :
     ?should_print_relations:
       (* ?event_env:(event env) -> ?expr_env:'a list list -> *)
       bool
  -> program
  -> (string, detailed_error list) result
(** [view_enabled ?event_env ?expr_env ?should_print_relations program]
    views the [program] by unpacking the events that are enabled in the program.
    @param event_env The environment to be used for events
    @param expr_env The environment to be used for expressions
    @param should_print_relations A flag to print relations
    @param program The program to be viewed
    @return The result of the view. *)

val unparse_program : program -> (string, detailed_error list) result
(** [unparse_program program] unparses the [program] and returns the result in string format.
    @param program The program to be unparsed
    @return The result of the unparse operation in string format. *)

type cmd_description =
  {name: string; alias: string; params: string list; desc: string}

val cmds : cmd_description list

val cmds_bbk_tree : string Misc.Bktree.bk_tree
