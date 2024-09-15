(* Monads for Result *)

let (>>=) x f = match x with Ok x -> f x | Error e -> Error e

let (>>!) x f = match x with Ok x -> Ok x | Error e -> f e

let (>>|) x f = x >>= fun x -> Ok (f x)

let fold_left_result f acc l = 
  List.fold_left (fun acc x -> acc >>= fun acc -> f acc x) (Ok acc) l

(* Monads for Option *)

let (>>?) x f = match x with Some x -> f x | None -> None

let (>>|?) x f = x >>? fun x -> Some (f x)
