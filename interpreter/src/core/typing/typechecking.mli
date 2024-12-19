open Ast
open Syntax
open Error
open Common
open Env

type event_kind = type_expr' * event_type'

type event_type_value = Undefined | Defined of event_kind

module EventTypes : sig
  module StringHashtbl : sig
    type key = string

    type 'a t = 'a Hashtbl.Make(String).t

    val create : int -> 'a t

    val clear : 'a t -> unit

    val reset : 'a t -> unit

    val copy : 'a t -> 'a t

    val add : 'a t -> key -> 'a -> unit

    val remove : 'a t -> key -> unit

    val find : 'a t -> key -> 'a

    val find_opt : 'a t -> key -> 'a option

    val find_all : 'a t -> key -> 'a list

    val replace : 'a t -> key -> 'a -> unit

    val mem : 'a t -> key -> bool

    val iter : (key -> 'a -> unit) -> 'a t -> unit

    val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit

    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

    val length : 'a t -> int

    val stats : 'a t -> Hashtbl.statistics

    val to_seq : 'a t -> (key * 'a) Seq.t

    val to_seq_keys : 'a t -> key Seq.t

    val to_seq_values : 'a t -> 'a Seq.t

    val add_seq : 'a t -> (key * 'a) Seq.t -> unit

    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit

    val of_seq : (key * 'a) Seq.t -> 'a t
  end

  val size : int

  val empty : event_type_value StringHashtbl.t

  val reset : 'a StringHashtbl.t -> unit

  val add : StringHashtbl.key * 'a -> 'a StringHashtbl.t -> 'a StringHashtbl.t

  val find : StringHashtbl.key -> 'a StringHashtbl.t -> 'a option

  val remove : StringHashtbl.key -> 'a StringHashtbl.t -> 'a StringHashtbl.t

  val show : (type_expr' * event_type') StringHashtbl.t -> string

  val to_list :
       event_type_value StringHashtbl.t
    -> (StringHashtbl.key * (type_expr' * event_type')) list
end

val typecheck :
     ?event_env:event' annotated env
  -> program
  -> (type_expr' env * event' annotated env, detailed_error list) result
(** [typecheck ?event_env program] typechecks the [program] by ensuring that:
    - All event expressions are well-typed according to the [event_env].
    - Template instance expressions are well-typed according to the [event_env]
      and [expr_env].
    - All relations find their corresponding events in the [event_env].

    @param event_env The environment of events to typecheck against.
    @param program The program to typecheck.
    @return
      A result containing the type environment and event environment if the
      program is well-typed, or a list of errors if the program is not
      well-typed. *)

val typecheck_expr :
     ?ty_env:type_expr' env
  -> ?label_types:event_type_value EventTypes.StringHashtbl.t
  -> expr
  -> (type_expr', detailed_error list) result
(** [typecheck_expr ?ty_env ?label_types expr] typechecks the [expr] by ensuring
    that:
    - All expressions are well-typed according to the [ty_env].
    - All event expressions are well-typed according to the [label_types].

    @param ty_env The environment of types to typecheck against.
    @param label_types The environment of event types to typecheck against.
    @param expr The expression to typecheck.
    @return
      A result containing the type of the expression if the expression is
      well-typed, or a list of errors if the expression is not well-typed. *)

val equal_types : type_expr' -> type_expr' -> bool
(** [equal_types ty1 ty2] checks if the two types [ty1] and [ty2] are equal. *)
