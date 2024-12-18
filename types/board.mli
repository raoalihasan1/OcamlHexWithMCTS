open! Core

type t [@@deriving bin_io]

val at_cell : t -> Move.t -> Color.t option
val empty : int -> t
val filter_available_moves : t -> Move.Set.t
val filter_available_moves_weighted : t -> Weighted_move.Set.t
val find_winner : t -> Color.t option
val make_move : t -> Move.t -> Color.t -> t
val of_string : int -> string -> t
val size : t -> int
