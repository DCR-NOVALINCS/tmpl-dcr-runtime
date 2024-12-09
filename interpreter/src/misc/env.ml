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
        let _ =
          Logger.error (Printf.sprintf "Duplicate binding for %s" x)
          (* Logger.error (Printf.sprintf "Size: %d" (List.length binds)) *)
        in
        Scope ((x, v) :: binds, sl) (* raise (Duplicate_binding x) *)
      else Scope ((x, v) :: binds, sl)

and find_flat x env =
  match env with
  | Empty -> None
  | Scope (binds, sl) -> (
    match List.assoc_opt x binds with
    | None -> find_flat x sl
    | _ as value -> value )

and get x env = Option.get (find_flat x env)

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
