open! Core

type t = Min_child | Max_child | Robust_child | Max_robust_child
[@@deriving of_string]

val find_branch : t -> Node.Minified.t ref -> Node.Minified.t ref option
