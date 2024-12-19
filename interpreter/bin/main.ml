open Cli

let _ = if !Sys.interactive then () else Exec.main ()
