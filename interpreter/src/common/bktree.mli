(** {1 BK-Tree}

    This module provides a simple implementation of a BK-tree data structure for
    storing strings and finding the nearest neighbor to a query string. *)

type 'a bk_tree =
  | Node of 'a * (int, 'a bk_tree) Hashtbl.t
      (** The type for a BK-tree node. *)

val create : string list -> string bk_tree
(** [create values] creates a new BK-tree from a list of values.
    @param values the list of values to create the BK-tree from.
    @return a new BK-tree of string value. *)

val add :
  (string -> string -> int) -> string bk_tree -> string -> string bk_tree
(** [add distance_fn tree value] adds an element to the BK-tree.
    @param distance_fn
      the function to calculate the distance between two strings.
    @param tree the BK-tree to add the value to.
    @param value the value to add to the BK-tree.
    @return the BK-tree with the value added. *)

val nearest_neighbor : ('a -> 'b -> int) -> 'a bk_tree -> 'b -> 'a * int
(** [nearest_neighbor ?distance_fn tree query] finds the nearest neighbor in the
    BK-tree.
    @param distance_fn
      the function to calculate the distance between two strings.
    @param tree the BK-tree to search for the nearest neighbor.
    @param query the query to find the nearest neighbor for.
    @return the nearest neighbor and the distance from the query. *)

val levenshtein_distance : string -> string -> int
(** [levenshtein_distance a b] calculates the Levenshtein distance between two
    strings.
    @param a the first string to calculate the distance for.
    @param b the second string to calculate the distance for.
    @return the Levenshtein distance between the two strings. *)
