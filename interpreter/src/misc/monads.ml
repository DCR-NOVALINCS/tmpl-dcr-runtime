module ResultMonad = struct
  (* type ('a, 'b) result = ('a, 'b) result *)

  let return x = Ok x

  let bind x f = match x with Ok x -> f x | Error e -> Error e

  let ( >>= ) = bind

  let ( >>| ) x f = x >>= fun x -> Ok (f x)

  let ( >>! ) x f = match x with Ok x -> Ok x | Error e -> f e

  let fold_left f acc l =
    List.fold_left (fun acc x -> acc >>= fun acc -> f acc x) (Ok acc) l

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
        >>= fun acc -> match f x with Some x -> Ok (x :: acc) | None -> Ok acc
        )
      l (return [])

  let iter f l = List.iter (fun x -> f x) l

  let partition f l = List.partition f l |> return

  let partition_map f l = List.partition_map f l |> return
end

module DetailedResultMonad = struct
  type ('v, 'w, 'e) detailed_result =
    {value: 'v; warnings: 'w list; errors: 'e list}

  let return ?(warnings = []) ?(errors = []) x = {value= x; warnings; errors}

  let return_ok x = return x

  let return_warning w x = return ~warnings:w x

  let return_error e x = return ~errors:e x

  let bind x f =
    match x with
    | {value= _; warnings= w; errors= e} as obj -> (
      match f obj with
      | {value= x; warnings= w'; errors= e'} ->
          {value= x; warnings= List.append w w'; errors= List.append e e'} )

  let has_errors x = List.length x.errors > 0

  let ( >>= ) = bind

  let ( >>| ) x f = x >>= fun x -> return (f x)

  let ( >>! ) x f =
    match x with
    | {value= _; warnings= w; errors= e} -> (
      match f e with
      | {value= x; warnings= w'; errors= e'} ->
          {value= x; warnings= w @ w'; errors= e @ e'} )

  let fold_left f acc l =
    List.fold_left (fun acc x -> acc >>= fun acc -> f acc x) (return acc) l

  let filter_map f l =
    List.fold_right
      (fun x acc -> match f x with Some x -> x :: acc | None -> acc)
      l []

  let iter f l = List.iter (fun x -> f x) l
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
