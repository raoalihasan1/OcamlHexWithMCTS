open! Core

type t

val diagonal : t -> Move.t list
val horizontal : t -> Move.t list
val of_move : Move.t -> board_size:int -> t
val vertical : t -> Move.t list
