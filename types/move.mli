open! Core

type t [@@deriving bin_io, compare, sexp, to_string]

val of_coordinates : int * int -> t
val of_string : string -> t
val to_coordinates : t -> int * int

include Comparable.S_binable with type t := t
