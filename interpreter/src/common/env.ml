open Printing

type 'a env = Empty | Scope of (string * 'a) list * 'a env [@@deriving show]

exception Not_found of string

exception Empty_env

exception Duplicate_binding of string

let rec empty_env = Scope ([], Empty)

(* let singleton_scope = [] *)

and begin_scope env = Scope ([], env)

and end_scope env =
  match env with Empty -> raise Empty_env | Scope (_, sl) -> sl

and bind x v env =
  match env with
  | Empty -> raise Empty_env
  | Scope (binds, sl) ->
      if List.mem_assoc x binds then
        let _ = Logger.warn (Printf.sprintf "Duplicate binding for %s" x) in
        Scope ((x, v) :: binds, sl) (* raise (Duplicate_binding x) *)
      else Scope ((x, v) :: binds, sl)

and bind_at_depth x v n env =
  let rec bind_at_depth' x v n i env =
    match (env, i) with
    | Empty, _ -> raise Empty_env
    | scope, i when i = n -> bind x v scope
    | Scope (binds, sl), i -> Scope (binds, bind_at_depth' x v n (i + 1) sl)
  in
  bind_at_depth' x v n 0 env

and find_flat x env =
  match env with
  | Empty -> None
  | Scope (binds, sl) -> (
    match List.assoc_opt x binds with
    | None -> find_flat x sl
    | _ as value -> value )

and get x env =
  match find_flat x env with None -> raise (Not_found x) | Some v -> v

and flatten env =
  let rec flatten' env acc =
    match env with
    | Empty -> acc
    | Scope (binds, sl) -> flatten' sl (List.append binds acc)
  in
  flatten' env []

and string_of_env v_fmt env =
  let rec string_of_scope sc =
    match sc with
    | [] -> ""
    | (x, v) :: [] -> string_of_bind x v
    | (x, v) :: sc ->
        Printf.sprintf "%s; %s" (string_of_bind x v) (string_of_scope sc)
  and string_of_bind x v = Printf.sprintf "%s = %s" x (v_fmt v) in
  match env with
  | Empty -> ""
  | Scope (sc, sl) ->
      "{ " ^ string_of_scope sc ^ " }" ^ "\n" ^ string_of_env v_fmt sl
