open Cli
module Cli = Cli

let () = if !Sys.interactive then () else Main.run ()
