open Monads.ResultMonad

let sanitize_input input =
  let tokens = String.split_on_char ' ' input in
  let tokens = List.filter (fun s -> s <> "") tokens in
  return tokens

let is_empty_string ?(map = fun s -> s) s =
  if String.trim s = "" then return (map s) else return s

let id x = Obj.magic x
