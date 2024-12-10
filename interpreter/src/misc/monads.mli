(** {1 Monads}

    Multiple implementations of monads for handling errors across functions. *)

(** {1 ResultMonad}

    An implementation of a monad "design pattern" for handling errors across
    functions, using the [Result] type. *)
module ResultMonad : sig
  val return : ?map_ok:('a -> 'a) -> 'a -> ('a, 'b) result
  (** [return ?map_ok x] wraps the value [x] in an [Ok] result. If [map_ok] is
      provided, it applies the function [f] to [x] before wrapping it.
      @param map_ok a function to apply to the value before wrapping it in [Ok].
      @param x the value to wrap.
      @return the value [x] wrapped in an [Ok] result. *)

  val fail : ?map_error:('a -> 'a) -> 'a -> ('b, 'a) result
  (** [fail ?map_error e] wraps the value [e] in an [Error] result. If
      [map_error] is provided, it applies the function [f] to [e] before
      wrapping it in [Error].
      @param map_error
        a function to apply to the value before wrapping it in [Error].
      @param e the value to wrap in [Error].
      @return the value [e] wrapped in an [Error] result. *)

  val bind : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  (** [bind m f] applies the function [f] to the value inside [m] if [m] is
      [Ok], otherwise returns the same [Error] value. {b Note:} This function
      doesn't accumulate errors.
      @param m the result to bind.
      @param f the function to apply to the value inside [m].
      @return the result of applying [f] to the value inside [m]. *)

  val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  (** [( >>= ) m f] is an infix operator for [bind]. *)

  val ( let* ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  (** [( let* ) m f] is a syntactic sugar for [bind] in monadic let binding. *)

  val apply : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
  (** [apply m f] applies the function [f] to the value inside [m] if [m] is
      [Ok], and wraps the result in [Ok].
      @param m the result to apply the function to.
      @param f the function to apply to the value inside [m].
      @return the result of applying [f] to the value inside [m]. *)

  val ( >>| ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
  (** [( >>| ) m f] is an infix operator for [apply]. *)

  val ( let+ ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
  (** [( let+ ) m f] is a syntactic sugar for [apply] in monadic let binding. *)

  val bind_error : ('a, 'b) result -> ('b -> ('a, 'c) result) -> ('a, 'c) result
  (** [bind_error m f] applies the function [f] to the value inside [m] if [m]
      is [Error], otherwise returns the [Ok] value.
      @param m the result to bind.
      @param f the function to apply to the value inside [m].
      @return the result of applying [f] to the value inside [m]. *)

  val ( >>! ) : ('a, 'b) result -> ('b -> ('a, 'c) result) -> ('a, 'c) result
  (** [( >>! ) m f] is an infix operator for [bind_error]. *)

  val fold_left :
    ('a -> 'b -> ('a, 'c) result) -> 'a -> 'b list -> ('a, 'c) result
  (** [fold_left f acc l] is a monadic fold left over the list [l] with the
      function [f], threading an accumulator of type [result].
      @param f the function to fold with.
      @param acc the initial accumulator.
      @param l the list to fold over.
      @return the result of folding over the list [l]. *)

  val fold_left2 :
       ('a -> 'b -> 'c -> ('a, 'd) result)
    -> 'a
    -> 'b list
    -> 'c list
    -> ('a, 'd) result
  (** [fold_left2 f acc l1 l2] folds the lists [l1] and [l2] from the left with
      the function [f], threading an accumulator of type [result].
      @param f the function to fold with.
      @param acc the initial accumulator.
      @param l1 the first list to fold over.
      @param l2 the second list to fold over.
      @return the result of folding over the lists [l1] and [l2]. *)

  val fold_right :
    ('a -> 'b -> ('a, 'c) result) -> 'a -> 'b list -> ('a, 'c) result
  (** [fold_right f acc l] folds the list [l] from the right with the function
      [f], threading an accumulator of type [result].
      @param f the function to fold with.
      @param acc the initial accumulator.
      @param l the list to fold over.
      @return the result of folding over the list [l]. *)

  val fold_right2 :
       ('a -> 'b -> 'c -> ('a, 'd) result)
    -> 'a
    -> 'b list
    -> 'c list
    -> ('a, 'd) result
  (** [fold_right2 f acc l1 l2] folds the lists [l1] and [l2] from the right
      with the function [f], threading an accumulator of type [result].
      @param f the function to fold with.
      @param acc the initial accumulator.
      @param l1 the first list to fold over.
      @param l2 the second list to fold over.
      @return the result of folding over the lists [l1] and [l2]. *)

  val map : ('a -> ('b, 'c) result) -> 'a list -> ('b list, 'c) result
  (** [map f l] maps the function [f] over the list [l], returning a result of
      type [result].
      @param f the function to map over the list.
      @param l the list to map the function over.
      @return the result of mapping the function over the list. *)

  val filter_map : ('a -> 'b option) -> 'a list -> ('b list, 'c) result
  (** [filter_map f l] maps the function [f] over the list [l], filtering out
      [None] values, and returning a result of type [result].
      @param f the function to map and filter over the list.
      @param l the list to map the function over.
      @return the result of mapping the function over the list. *)

  val iter : ('a -> (unit, 'b) result) -> 'a list -> (unit, 'b) result
  (** [iter f l] applies the function [f] to each element of the list [l],
      returning a result of type [result] wrapping unit value.
      @param f the function to apply to each element of the list.
      @param l the list to apply the function to.
      @return the result of applying the function to each element of the list. *)

  val partition : ('a -> bool) -> 'a list -> ('a list * 'a list, 'b) result
  (** [partition f l] partitions the list [l] into two lists based on the
      predicate [f], returning a result of type [result].
      @param f the predicate to partition the list with.
      @param l the list to partition.
      @return the result of partitioning the list. *)

  val partition_map :
    ('a -> ('b, 'c) Either.t) -> 'a list -> ('b list * 'c list, 'd) result
  (** [partition_map f l] partitions the list [l] into two lists based on the
      function [f] which returns an [Either] type, returning a result of type
      [result].
      @param f the function to partition the list with.
      @param l the list to partition.
      @return the result of partitioning the list. *)
end

module OptionMonad : sig
  val return : 'a -> 'a option

  val bind : 'a option -> ('a -> 'b option) -> 'b option

  val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option

  val ( >>| ) : 'a option -> ('a -> 'b) -> 'b option

  val ( >>! ) : 'a option -> 'a option -> 'a option

  val fold_left : ('a -> 'b -> 'a option) -> 'a -> 'b list -> 'a option

  val map : ('a -> 'b option) -> 'a option list -> 'b option list

  val iter : ('a -> unit) -> 'a list -> unit
end

module FilterMonad : sig
  type 'a filter_wrapper = 'a

  val return : 'a -> 'a filter_wrapper

  val get : 'a filter_wrapper -> 'a

  val bind :
    'a filter_wrapper -> bool -> ('a -> 'a filter_wrapper) -> 'a filter_wrapper

  val ( >>= ) :
       'a filter_wrapper filter_wrapper
    -> bool * ('a -> 'a filter_wrapper)
    -> 'a filter_wrapper filter_wrapper

  val ( >>| ) : 'a -> ('a -> 'b) -> 'b

  val ( >>! ) : 'a -> 'b -> 'a

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

  val map : ('a -> 'b) -> 'a list -> 'b list

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list

  val iter : ('a -> unit) -> 'a list -> unit
end
