open! Core

module T = struct
  type t = float * Move.t [@@deriving compare, sexp]
end

let create move =
  let x, y = Move.to_coordinates move in
  let weight =
    let f num = (0.125 *. (num -. 5.0)) ** 2.0
    and x_float = float_of_int x
    and y_float = float_of_int y in
    2.0 ** (-8.0 *. (f x_float +. f y_float))
  in
  (weight, move)

let move = snd
let weight = fst

include T
include Comparable.Make (T)

let%expect_test "Computes the weight of a move" =
  let weighted_move = create (Move.of_coordinates (5, 6)) in
  print_s [%message (weighted_move : t)];
  [%expect {| (weighted_move (0.91700404320467122 (5 6))) |}]
