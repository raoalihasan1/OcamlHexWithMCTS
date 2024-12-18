open! Core

type t [@@deriving bin_io]

val create : visits:int -> reward:float -> t
val reward : t -> float
val visits : t -> int
