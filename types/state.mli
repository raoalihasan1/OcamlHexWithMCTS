open! Core

type t [@@deriving bin_io]

val board : t -> Board.t
val create : board:Board.t -> current_player:Color.t -> move_made:Move.t -> t
val current_player : t -> Color.t
val empty : board_size:int -> t
val move_made : t -> Move.t
val switch_player : t -> t
val update_board : t -> Board.t -> t
val update_move_made : t -> Move.t -> t
