open State
open Cmdliner

let quit_cmd (_ : runtime_state) = exit 0

let term = Term.(const quit_cmd)
