open! Core

type t [@@deriving sexp_of]

val create : Move.t -> t
val move : t -> Move.t
val weight : t -> float

include Comparable.S with type t := t
