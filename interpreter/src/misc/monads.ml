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

  let bind_error m f = match m with Error e -> f e | Ok x -> Ok x

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
end

module ProgramState = struct
  type ('a, 'e) result_program = {value: 'a; errors: 'e list}

  let return ?(errors = []) x = {value= x; errors}

  let fail ?(value = ()) e = {value= Obj.magic value; errors= [e]}

  let bind {value= v; errors= e} f =
    let {value= v'; errors= e'} = f v in
    {value= v'; errors= List.append e e'}

  let ( >>= ) = bind

  let apply {value= v; errors= e} f = {value= f v; errors= e}

  let ( >>| ) = apply

  let apply_error {value= v; errors= e} f = {value= v; errors= f e}

  let ( >>! ) = apply_error

  let fold_left f acc l =
    List.fold_left (fun m x -> m >>= fun acc -> f acc x) (return acc) l

  let map f l =
    List.fold_right
      (fun x {value= acc; errors= e} ->
        let {value= x'; errors= e'} = f x in
        return ~errors:(List.append e e') (x' :: acc) )
      l (return [])

  let filter_map f l =
    List.fold_right
      (fun x {value= acc; errors} ->
        match f x with
        | {value; errors= e} -> (
          match value with
          | Some v -> return ~errors:(List.append errors e) (v :: acc)
          | None -> return ~errors:(List.append errors e) acc ) )
      l (return [])

  let iter f l =
    List.fold_left
      (fun {errors= e; _} x ->
        let {errors= e'; _} = f x in
        return ~errors:(List.append e e') () )
      (return ()) l

  let partition f l = return @@ List.partition f l

  let partition_map f l = return @@ List.partition_map f l
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
