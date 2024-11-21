module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
end

module ResultMonad = struct
  (* type ('a, 'b) result = ('a, 'b) result *)

  let return x = Ok x

  let fail e = Error e

  let bind x f = match x with Ok x -> f x | Error e -> Error e

  let ( >>= ) = bind

  let ( >>| ) x f = x >>= fun x -> return (f x)

  let ( >>! ) x f = match x with Ok x -> return x | Error e -> f e

  let fold_left f acc l =
    List.fold_left (fun acc x -> acc >>= fun acc -> f acc x) (return acc) l

  let map f l =
    List.fold_right
      (fun x acc -> acc >>= fun acc -> f x >>| fun x -> x :: acc)
      l (return [])

  let filter_map f l =
    (* List.fold_right (fun x acc -> match f x with Some x -> x :: acc | None ->
       acc) l [] *)
    List.fold_right
      (fun x acc ->
        acc
        >>= fun acc ->
        match f x with Some x -> return (x :: acc) | None -> return acc )
      l (return [])

  let iter f l =
    List.fold_left (fun acc x -> acc >>= fun _ -> f x) (return ()) l

  let partition f l = List.partition f l |> return

  let partition_map f l = List.partition_map f l |> return
end

module ProgramResultMonad = struct
  (** Type definition for the monad *)
  type ('a, 'e, 'c) state = {value: 'a; errors: 'e list; context: 'c}

  (** [return ?errors ~context value] wraps a {b value} in the monad
      @param errors A list of errors
      @param context The context of the value that is stored in the monad
      @param value The value to be wrapped in the monad
      @return A monad with the value *)
  let return ?(errors = []) ~context value = {value; errors; context}

  (** [fail ?errors ~context value], same as [return] but with errors
      @param errors A list of errors
      @param context The context of the value that is stored in the monad
      @param value The value to be wrapped in the monad
      @return A monad with the errors *)
  let fail ?(errors = []) ~context value = {value; errors; context}

  (** [bind x f] applies f to the value of [x]. If [x] is an error, it returns
      [x] as is
      @param x The monad to apply the function to
      @param f The function to apply to the monad
      @return The result of applying the function to the monad *)
  let bind x f =
    match x with
    | {value; errors; context} -> (
      match f value context with
      | {value; errors= e; context} -> {value; errors= errors @ e; context} )

  (** [get_errors] returns the errors of the monad
      @param x The monad to get the errors from
      @return The errors of the monad *)
  let get_errors {errors; _} = errors

  let get_value {value; _} = value

  (** [>>=] is an infix operator for [bind] *)
  let ( >>= ) = bind

  (** [apply x f] applies the function [f] to the value of [x]
      @param x The monad to apply the function to
      @param f The function to apply to the monad
      @return The result of applying the function to the monad *)
  let apply x f = x >>= fun value context -> return ~context (f value context)

  (** [>>|] is an infix operator for [apply] *)
  let ( >>| ) = apply

  let bind_error x f =
    match x with {value; errors; context} -> {value; errors= f errors; context}

  let ( >>! ) = bind_error

  (** [fold_left f acc l] applies the function [f] to the accumulator and each
      element of the list [l]
      @param f
        The function to apply to the accumulator and each element of the list
      @param acc The initial accumulator
      @param l The list to apply the function to
      @return The result of applying the function to the list *)
  let fold_left ~context f acc l =
    List.fold_left
      (fun acc x -> acc >>= fun acc -> f acc x)
      (return ~context acc) l

  (** [map f l] applies the function [f] to each element of the list [l]
      @param f The function to apply to each element of the list
      @param l The list to apply the function to
      @return The result of applying the function to the list *)
  let map ~context f l =
    List.fold_right
      (fun x acc ->
        acc >>= fun acc context -> f x context >>| fun x _ -> x :: acc )
      l (return ~context [])

  (** [filter_map f l] applies the function [f] to each element of the list [l]
      and filters out the elements that are [None]
      @param f The function to apply to each element of the list
      @param l The list to apply the function to
      @return The result of applying the function to the list *)
  let filter_map ~context f l =
    List.fold_right
      (fun x acc ->
        acc
        >>= fun acc context ->
        match f x context with
        | Some x -> return ~context (x :: acc)
        | None -> return ~context acc )
      l (return ~context [])

  (** [iter f l] applies the function [f] to each element of the list [l]
      @param f The function to apply to each element of the list
      @param l The list to apply the function to *)
  let iter ~context f l =
    List.fold_left
      (fun acc x -> acc >>= fun _ context -> f x context)
      (return ~context ()) l

  (** [partition_map f l] applies the function [f] to each element of the list
      [l] and partitions the list into two lists based on the result of the
      function
      @param f The function to apply to each element of the list
      @param l The list to apply the function to
      @return The result of applying the function to the list *)
  let partition_map ~context f l =
    List.partition_map (fun x -> f x context) l
    |> fun (l, r) -> return ~context (l, r)

  (** [partition f l] partitions the list [l] into two lists based on the
      predicate [f]
      @param f The predicate to partition the list with
      @param l The list to partition
      @return The result of partitioning the list *)
  let partition ~context f l =
    List.partition f l |> fun (l, r) -> return ~context (l, r)
end

module OptionMonad = struct
  let return x = Some x

  let bind x f = match x with Some x -> f x | None -> None

  let ( >>= ) = bind

  let ( >>| ) x f = x >>= fun x -> Some (f x)

  let ( >>! ) x f = match x with Some x -> Some x | None -> f

  let fold_left f acc l =
    List.fold_left (fun acc x -> acc >>= fun acc -> f acc x) (Some acc) l

  let map f l = List.map (fun x -> x >>= fun x -> f x >>| fun x -> x) l

  let iter f l = List.iter f l
end

module FilterMonad = struct
  (* Type definition for the monad *)
  type 'a filter_wrapper = 'a

  (* Return function: wraps a value in the monad *)
  let return (x : 'a) : 'a filter_wrapper = x

  let get (x : 'a filter_wrapper) : 'a = x

  (* Bind function: applies f to x if the condition is true, otherwise returns
     x *)
  let bind (x : 'a filter_wrapper) (condition : bool)
      (f : 'a -> 'a filter_wrapper) : 'a filter_wrapper =
    if condition then f x else x

  (* Infix operator for bind: allows chaining operations in a monadic style *)
  let ( >>= ) (x : 'a filter_wrapper) (condition, f) : 'a filter_wrapper =
    bind x condition f

  let ( >>| ) x f = f x

  let ( >>! ) x _f = x

  let fold_left f acc l = List.fold_left (fun acc x -> f acc x) acc l

  let map f l = List.map f l

  let filter_map f l = List.filter_map f l

  let iter f l = List.iter (fun x -> f x) l
end
