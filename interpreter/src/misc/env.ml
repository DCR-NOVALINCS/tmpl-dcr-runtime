type 'a env = (string * 'a) list list

let empty_env = [[]]

let empty_list = []

let singleton_scope = [[]]

(**TODO: this bind don't stop if want to put to values with the same key*)
let bind x v env = ((x, v) :: List.hd env) :: List.tl env

let or_else v1 v2 = match v1 with None -> v2 () | Some v -> Some v

let rec find n x env =
  match env with
  | [] -> None
  | sc :: env ->
      or_else
        (Option.bind (List.assoc_opt x sc) (fun v -> Option.some (v, n)))
        (fun () -> find (n + 1) x env)

let find_flat x env = Option.bind (find 1 x env) (fun v -> Option.some (fst v))

let get x env = Option.get (find 1 x env)

let begin_scope env = [] :: env

let end_scope env = match env with [] -> assert false | _ :: env -> env

let decrease_nesting (x, (x', n)) = (x, (x', n - 1))

let string_of_env v_fmt env =
  let env_len = List.length env in
  let rec string_of_scope sc =
    match sc with
    | [] -> ""
    | (x, v) :: sc -> x ^ " = " ^ v_fmt v ^ "; " ^ string_of_scope sc
  in
  let rec string_of_env' n env =
    match env with
    | [] -> ""
    | sc :: env ->
        "Scope "
        ^ string_of_int (env_len - n)
        ^ ": { " ^ string_of_scope sc ^ " }" ^ "\n"
        ^ string_of_env' (n + 1) env
  in
  string_of_env' 1 env

let flatten env = List.flatten env
