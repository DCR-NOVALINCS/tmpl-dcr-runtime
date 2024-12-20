let unparse_list ?(initial = "") ?(separator = "") ?(buffer = Buffer.create 100)
    unparse_fn list =
  match list with
  | [] -> ()
  | _ ->
      Buffer.add_string buffer @@ initial ;
      let rec unparse_list_aux = function
        | [] -> ()
        | [x] -> unparse_fn ~buffer x
        | x :: xs ->
            unparse_fn ~buffer x ;
            Buffer.add_string buffer @@ separator ;
            unparse_list_aux xs
      in
      unparse_list_aux list
