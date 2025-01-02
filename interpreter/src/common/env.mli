(** {1 Environment}

    This module provides a simple environment data structure for storing
    bindings between keys and values. The environment supports adding and
    removing scopes, binding values to keys, and finding values bound to keys. *)

(** [Not_found id] is raised when a binding for [id] is not found *)
exception Not_found of string

(** [Empty_env] is raised when an operation is performed on an empty environment *)
exception Empty_env

(** [Duplicate_binding id] is raised when a binding for [id] already exists *)
exception Duplicate_binding of string

type 'a env = Empty | Scope of (string * 'a) list * 'a env

val empty_env : 'a env
(** [empty_env] returns an empty environment with no bindings
    @return an empty environment *)

val begin_scope : 'a env -> 'a env
(** [begin_scope env] returns a new environment with an empty scope
    @param env the environment to add a new scope to
    @return a new environment with an empty scope *)

val end_scope : 'a env -> 'a env
(** [end_scope env] returns the environment without the last scope
    @param env the environment to remove the last scope from
    @return the environment without the last scope
    @raise Empty_env if the environment is empty *)

val bind : string -> 'a -> 'a env -> 'a env
(** [bind x v env] binds the value [v] to the key [x] in the environment [env]
    @param x the key to bind the value to
    @param v the value to bind to the key
    @param env the environment to bind the value to the key
    @return the environment with the value bound to the key *)

val bind_at_depth : string -> 'a -> int -> 'a env -> 'a env
(** [bind_at_depth x v n env] binds the value [v] to the key [x] in the [n]th
    scope of the environment [env]
    @param x the key to bind the value to
    @param v the value to bind to the key
    @param n the depth of the scope to bind the value in
    @param env the environment to bind the value to the key
    @return the environment with the value bound to the key *)

val find_flat : string -> 'a env -> 'a option
(** [find_flat x env] returns the value bound to the key [x] in the environment
    [env]
    @param x the key to find the value for
    @param env the environment to find the value in
    @return the value bound to the key *)

val get : string -> 'a env -> 'a
(** [get x env] returns the value bound to the key [x] in the environment [env]
    @param x the key to find the value for
    @param env the environment to find the value in
    @return the value bound to the key
    @raise Not_found if the key is not found *)

val flatten : 'a env -> (string * 'a) list
(** [flatten env] returns a list of all the bindings in the environment [env]
    @param env the environment to flatten
    @return a list of all the bindings in the environment *)

(* val map : ('a -> 'b) -> 'a env -> 'b env *)

val string_of_env : ('a -> string) -> 'a env -> string
(** [string_of_env v_fmt env] returns a string representation of the environment
    [env]
    @param v_fmt the function to format the values in the environment
    @param env the environment to format
    @return a string representation of the environment *)
