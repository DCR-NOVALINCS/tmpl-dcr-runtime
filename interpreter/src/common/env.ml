open Printing

type 'a env = Empty | Scope of (string * 'a) list * 'a env

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
      if List.mem_assoc x binds then (
        Logger.warn (Printf.sprintf "Duplicate binding for %s" (keyword x)) ;
        Scope ((x, v) :: binds, sl) (* raise (Duplicate_binding x) *) )
      else Scope ((x, v) :: binds, sl)

and bind_at_depth x v depth env =
  let rec bind_at_depth' x v n i env =
    match env with
    | Empty -> raise Empty_env
    | scope when i = n -> bind x v scope
    | Scope (binds, sl) -> Scope (binds, bind_at_depth' x v n (i + 1) sl)
  in
  bind_at_depth' x v depth 0 env

and find_flat x env =
  match env with
  | Empty ->
      Logger.warn (Printf.sprintf "Key %s not found" (keyword x)) ;
      None
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

(* and map f env =
   match env with
   | Empty -> Empty
   | Scope (binds, sl) ->
       Scope (List.map (fun (k, v) -> (k, f v)) binds, map f sl) *)

and string_of_env v_fmt env =
  let rec string_of_bind (x, v) = Printf.sprintf "%s = %s" x (v_fmt v)
  and string_of_env' depth env =
    match env with
    | Empty -> ""
    | Scope (binds, sl) ->
        let binds_str = List.map string_of_bind binds in
        let binds_str = String.concat ", " binds_str in
        let sl_str = string_of_env' (depth + 1) sl in
        Printf.sprintf "%d: { %s }\n%s" depth binds_str sl_str
  in
  string_of_env' 0 env
