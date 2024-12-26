module type Spec = sig
  type node

  type edge

  val compare : node -> node -> int

  val show_node : node -> string

  val show_edge : edge -> string
end

module Make (S : Spec) = struct
  module NodeMap = Map.Make (struct
    type t = S.node

    let compare = S.compare
  end)

  type graph = (S.edge * S.node) list NodeMap.t

  let empty : graph = NodeMap.empty

  let add_node g node = if NodeMap.mem node g then g else NodeMap.add node [] g

  let add_edge g from_node (to_node : S.node) (edge : S.edge) =
    let neighbors =
      match NodeMap.find_opt from_node g with
      | Some edges -> (edge, to_node) :: edges
      | None -> [(edge, to_node)]
    in
    NodeMap.add from_node neighbors g

  let neighbors g node = NodeMap.find_opt node g |> Option.value ~default:[]

  let map g f =
    NodeMap.map
      (fun edges -> List.map (fun (node, edge) -> (f node, edge)) edges)
      g

  let find_node id g = NodeMap.find_opt id g

  let keys g = NodeMap.bindings g |> List.map fst

  let edges g = NodeMap.bindings g |> List.map snd |> List.concat
end
