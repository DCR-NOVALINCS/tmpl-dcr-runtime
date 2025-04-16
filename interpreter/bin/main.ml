open Cli

let start = if !Sys.interactive then () else Main.run ()

let () = start
