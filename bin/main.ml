open Templating

let () = 
let open Syntax in
let open Instantiation in 
  let program = { template_decls = []
                ; events = []
                ; template_insts = []
                ; relations = []
                } in
  let _ = instantiate program in
  print_endline "Hello, World!"

