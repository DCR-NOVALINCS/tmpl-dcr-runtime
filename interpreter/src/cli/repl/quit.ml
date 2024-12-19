open State
open Cmdliner

let quit_cmd state =
  let {output; _} = state in
  print_endline output ; exit 0

let term = Term.(const quit_cmd)
