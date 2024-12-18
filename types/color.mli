open! Core

type t = Red | Blue
[@@deriving bin_io, enumerate, equal, of_string, sexp_of, variants]

val opponent : t -> t
val to_weight : t -> int
