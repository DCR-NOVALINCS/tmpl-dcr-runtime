(* Monads for Result *)

let bind x f = match x with Ok x -> f x | Error e -> Error e

let (>>=) x f = bind x f

let (>>!) x f = match x with Ok x -> Ok x | Error e -> f e

let (>>|) x f = x >>= fun x -> Ok (f x) 

let return x = Ok x

let fold_left_result f acc l = 
  List.fold_left (fun acc x -> acc >>= fun acc -> f acc x) (Ok acc) l

let map_result f l = 
  List.fold_right (fun x acc -> acc >>= fun acc -> f x >>| fun x -> x::acc) l (Ok [])

let filter_map f l = 
  List.fold_right (fun x acc -> match f x with Some x -> x::acc | None -> acc) l []

(* Monads for Option *)

let (>>?) x f = match x with Some x -> f x | None -> None

let (>>|?) x f = x >>? fun x -> Some (f x)

(* Modules *)

module FilterMonad = struct
  (* Type definition for the monad *)
  type 'a t = 'a

  (* Return function: wraps a value in the monad *)
  let return (x : 'a) : 'a t = x

  let get (x : 'a t) : 'a = x

  (* Bind function: applies f to x if the condition is true, otherwise returns x *)
  let bind (x : 'a t) (condition : bool) (f : 'a -> 'a t) : 'a t =
    if condition then f x else x

  (* Infix operator for bind: allows chaining operations in a monadic style *)
  let ( >>= ) (x : 'a t) (condition, f) : 'a t = bind x condition f
end
