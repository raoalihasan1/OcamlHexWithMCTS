open! Core

module Minified : sig
  type t [@@deriving bin_io]

  val average_reward : t -> float
  val children : t -> t ref list
  val move_made : t -> Move.t
  val no_of_times_visited : t -> int
end

type t [@@deriving bin_io]

val children : t -> t ref list
val create : state:State.t -> t
val increment_reward : t -> t
val no_of_times_visited : t -> int
val rave_stats : t -> Rave.t Move.Map.t
val reward : t -> float
val state : t -> State.t
val to_minified : t -> n0:float -> Minified.t
val update_children : t -> t ref list -> t
val update_rave_stats : t -> Move.t -> bool -> t
val update_state : t -> State.t -> t
val visit : t -> t
