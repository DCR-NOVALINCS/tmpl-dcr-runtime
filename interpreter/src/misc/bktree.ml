(* Define the type for the BK-tree node *)
type 'a bk_tree = Node of 'a * (int, 'a bk_tree) Hashtbl.t

(* Function to create a new BK-tree from a list of values *)
let rec create values =
  match values with
  | [] -> failwith "Cannot create a BK-tree with no values"
  | hd :: tl ->
      let tree = Node (hd, Hashtbl.create 10) in
      List.fold_left
        (fun acc value -> add levenshtein_distance acc value)
        tree tl

(* Function to add elements to the BK-tree *)
and add distance_fn tree value =
  match tree with
  | Node (v, children) ->
      let d = distance_fn v value in
      if Hashtbl.mem children d then
        let child = Hashtbl.find children d in
        Hashtbl.replace children d (add distance_fn child value)
      else Hashtbl.add children d (create [value]) ;
      tree

(* Function to find the nearest neighbor *)
and nearest_neighbor distance_fn tree query =
  let rec aux (best, best_dist) (Node (v, children)) =
    let d = distance_fn v query in
    let best, best_dist =
      if d < best_dist then (v, d) else (best, best_dist)
    in
    Hashtbl.fold
      (fun dist child (best, best_dist) ->
        if dist >= d - best_dist && dist <= d + best_dist then
          aux (best, best_dist) child
        else (best, best_dist) )
      children (best, best_dist)
  in
  match tree with
  | Node (v, _children) -> aux (v, distance_fn v query) tree

(* Example usage *)
and levenshtein_distance a b =
  (* Example: Levenshtein distance for strings *)
  let rec aux i j =
    if i = 0 then j
    else if j = 0 then i
    else
      let cost = if a.[i - 1] = b.[j - 1] then 0 else 1 in
      min
        (min (aux (i - 1) j + 1) (aux i (j - 1) + 1))
        (aux (i - 1) (j - 1) + cost)
  in
  aux (String.length a) (String.length b)

(* let () = let values = ["hello"; "hallo"; "hullo"; "hell"; "help"] in let
   tree = create values in let (nearest, dist) = nearest_neighbor
   levenshtein_distance tree "hell" in Printf.printf "Nearest neighbor: %s
   with distance: %d\n" nearest dist *)
