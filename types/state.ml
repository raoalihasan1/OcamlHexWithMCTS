open! Core

type t = { board : Board.t; current_player : Color.t; move_made : Move.t }
[@@deriving bin_io, fields ~getters ~iterators:create]

let create = Fields.create

let empty ~board_size =
  create ~board:(Board.empty board_size)
    ~current_player:Red
      (* -1,-1 is only the case for the root node as
         the board is empty since no move is made *)
    ~move_made:(Move.of_coordinates (-1, -1))

let switch_player t =
  { t with current_player = Color.opponent t.current_player }

let update_board t board = { t with board }
let update_move_made t move_made = { t with move_made }
