open Cli

let () = if !Sys.interactive then () else Main.run ()
