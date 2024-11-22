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

module ProgramState = struct
  type ('l, 'e) context = {logs: 'l list; errors: 'e list}

  let empty_context = {logs= []; errors= []}

  type ('a, 'l, 'e) state = ('l, 'e) context -> 'a option * ('l, 'e) context

  let return ?(errors = []) ?(logs = []) x s =
    (x, {errors= List.append s.errors errors; logs= List.append s.logs logs})

  let fail e s = return ~errors:[e] None s

  let log l s = return ~logs:[l] None s

  let get state = (state, state)

  let put new_state _ = (None, new_state)

  let bind m f s =
    let x, s' = m s in
    let x', s'' = f x s' in
    (x', s'')

  let ( >>= ) = bind

  let ( let* ) = bind

  let apply m f = m >>= fun x -> return (f x)

  let ( >>| ) = apply

  let fold_left f acc l =
    List.fold_left (fun acc x -> acc >>= fun acc -> f acc x) (return acc) l

  let map f l = List.map (fun x -> x >>= fun x -> f x >>| fun x -> x) l

  let filter_map f l = List.filter_map f l

  let iter f l = List.iter f l

  let partition_map f l = List.partition_map f l |> return

  let partition f l = List.partition f l |> return
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
