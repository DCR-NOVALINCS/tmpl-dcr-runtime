module ResultMonad = struct
  (* type ('a, 'b) result = ('a, 'b) result *)

  let return ?map_ok x = match map_ok with Some f -> Ok (f x) | None -> Ok x

  let fail ?map_error e =
    match map_error with Some f -> Error (f e) | None -> Error e

  let bind m f = match m with Ok x -> f x | Error e -> Error e

  let ( >>= ) = bind

  let ( let* ) = bind

  let apply m f = m >>= fun x -> return (f x)

  let ( >>| ) = apply

  let ( let+ ) = apply

  let compose m f g =
    let m = bind m f in
    bind m g

  let ( >=> ) = compose

  let bind_error m f = match m with Error e -> f e | Ok _ as ok -> ok

  let ( >>! ) = bind_error

  let fold_left f acc l =
    List.fold_left (fun acc x -> acc >>= fun acc -> f acc x) (return acc) l

  let fold_left2 f acc l1 l2 =
    List.fold_left2
      (fun acc x1 x2 -> acc >>= fun acc -> f acc x1 x2)
      (return acc) l1 l2

  let fold_right f acc l =
    List.fold_right (fun x acc -> acc >>= fun acc -> f acc x) l (return acc)

  let fold_right2 f acc l1 l2 =
    List.fold_right2
      (fun x1 x2 acc -> acc >>= fun acc -> f acc x1 x2)
      l1 l2 (return acc)

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

  let for_all f l =
    fold_left (fun acc x -> f x >>= fun v -> return (v && acc)) true l

  (* let exists f l =
    fold_left (fun acc x -> f x >>= fun v -> return (v || acc)) false l *)
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
