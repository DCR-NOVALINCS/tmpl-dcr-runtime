
let (>>=) x f = match x with Ok x -> f x | Error e -> Error e

let (>>|) x f = x >>= fun x -> Ok (f x)

let fold_left_result f acc l = 
  List.fold_left (fun acc x -> acc >>= fun acc -> f acc x) (Ok acc) l
