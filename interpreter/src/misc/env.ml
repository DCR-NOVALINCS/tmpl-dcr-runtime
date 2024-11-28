(* type 'a env = (string * 'a) list list

   let empty_env = [[]]

   let empty_list = []

   let singleton_scope = [[]]

   (**TODO: this bind don't stop if want to put to values with the same key*)
   let bind x v env = ((x, v) :: List.hd env) :: List.tl env

   let rec find n x env =
     let or_else v1 v2 = match v1 with None -> v2 () | Some v -> Some v in
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

   let flatten env = List.flatten env *)

type 'a env = Empty | Scope of (string * 'a) list * 'a env

exception Not_found

exception Empty_env

exception Duplicate_binding

let empty_env = Scope ([], Empty)

(* let singleton_scope = [] *)

let begin_scope env = Scope ([], env)

let end_scope env =
  match env with Empty -> raise Empty_env | Scope (_, sl) -> sl

let bind x v env =
  match env with
  | Empty -> raise Empty_env
  | Scope (binds, sl) ->
      (* if List.mem_assoc x binds then raise Duplicate_binding
         else *)
      Scope ((x, v) :: binds, sl)

let rec find_flat x env =
  match env with
  | Empty -> None
  | Scope (binds, sl) -> (
    match List.assoc_opt x binds with
    | Some v -> Some v
    | None -> find_flat x sl )

let get x env = Option.get (find_flat x env)

let flatten env =
  let rec flatten' env acc =
    match env with
    | Empty -> acc
    | Scope (binds, sl) -> flatten' sl (List.append binds acc)
  in
  flatten' env []

let rec string_of_env v_fmt env =
  let rec string_of_scope sc =
    match sc with
    | [] -> ""
    | (x, v) :: sc -> x ^ " = " ^ v_fmt v ^ "; " ^ string_of_scope sc
  in
  match env with
  | Empty -> ""
  | Scope (sc, sl) ->
      "Scope { " ^ string_of_scope sc ^ " }" ^ "\n" ^ string_of_env v_fmt sl
