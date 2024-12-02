open Monads.ResultMonad

let sanitize_input input =
  input |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") |> return
