module ResultMonad = struct
  type ('a, 'b) t = ('a, 'b) result

  let return (x : 'a) : ('a, 'b) t = Ok x

  let bind (x : ('a, 'b) t) (f : 'a -> ('c, 'b) t) : ('c, 'b) t =
    match x with Ok x -> f x | Error e -> Error e

  let ( >>= ) (x : ('a, 'b) t) (f : 'a -> ('c, 'b) t) : ('c, 'b) t =
    bind x f

  let ( >>| ) x f = x >>= fun x -> Ok (f x)

  let ( >>! ) x f = match x with Ok x -> Ok x | Error e -> f e

  let fold_left f acc l =
    List.fold_left (fun acc x -> acc >>= fun acc -> f acc x) (Ok acc) l

  let map f l =
    List.fold_right
      (fun x acc -> acc >>= fun acc -> f x >>| fun x -> x :: acc)
      l (Ok [])

  let filter_map f l =
    List.fold_right
      (fun x acc -> match f x with Some x -> x :: acc | None -> acc)
      l []

  let iter f l = List.iter (fun x -> f x) l
end

module OptionMonad = struct
  type 'a t = 'a option

  let return (x : 'a) : 'a t = Some x

  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
    match x with Some x -> f x | None -> None

  let ( >>= ) (x : 'a t) (f : 'a -> 'b t) : 'b t = bind x f

  let ( >>| ) x f = x >>= fun x -> Some (f x)

  let ( >>! ) x f = match x with Some x -> Some x | None -> f

  let fold_left f acc l =
    List.fold_left (fun acc x -> acc >>= fun acc -> f acc x) (Some acc) l

  let map f l =
    List.fold_right
      (fun x acc -> acc >>= fun acc -> f x >>| fun x -> x :: acc)
      l (Some [])

  let filter_map f l =
    List.fold_right
      (fun x acc -> match f x with Some x -> x :: acc | None -> acc)
      l []

  let iter f l = List.iter (fun x -> f x) l
end

module FilterMonad = struct
  (* Type definition for the monad *)
  type 'a t = 'a

  (* Return function: wraps a value in the monad *)
  let return (x : 'a) : 'a t = x

  let get (x : 'a t) : 'a = x

  (* Bind function: applies f to x if the condition is true, otherwise
     returns x *)
  let bind (x : 'a t) (condition : bool) (f : 'a -> 'a t) : 'a t =
    if condition then f x else x

  (* Infix operator for bind: allows chaining operations in a monadic
     style *)
  let ( >>= ) (x : 'a t) (condition, f) : 'a t = bind x condition f

  let ( >>| ) x f = f x

  let ( >>! ) x _f = x

  let fold_left f acc l = List.fold_left (fun acc x -> f acc x) acc l

  let map f l = List.map f l

  let filter_map f l =
    List.fold_right
      (fun x acc -> match f x with Some x -> x :: acc | None -> acc)
      l []

  let iter f l = List.iter (fun x -> f x) l
end
