open Monads.ResultMonad

let sanitize_input input =
  let tokens = String.split_on_char ' ' input in
  let tokens = List.filter (fun s -> s <> "") tokens in
  return tokens
