open! Core

type t = {
  diagonal : Move.t list;
  horizontal : Move.t list;
  vertical : Move.t list;
}
[@@deriving fields ~getters, sexp_of]

let of_move move ~board_size =
  let x, y = Move.to_coordinates move in
  let horizontal_dirs = [ (0, -1); (0, 1) ] in
  let vertical_dirs = [ (-1, 0); (1, 0) ] in
  let diagonal_dirs = [ (-1, 1); (1, -1) ] in
  let compute_neighbors directions =
    List.filter
      (List.map directions ~f:(fun (r, c) -> (x + r, y + c)))
      ~f:(fun (r, c) -> r >= 0 && r < board_size && c >= 0 && c < board_size)
    |> List.map ~f:Move.of_coordinates
  in
  {
    horizontal = compute_neighbors horizontal_dirs;
    vertical = compute_neighbors vertical_dirs;
    diagonal = compute_neighbors diagonal_dirs;
  }

let%expect_test "Returns the neighbour co-ordinates of board position (1, 1)" =
  let t = of_move (Move.of_coordinates (1, 1)) ~board_size:5 in
  print_s [%message (t : t)];
  [%expect
    {|
    (t
     ((diagonal ((0 2) (2 0))) (horizontal ((1 0) (1 2)))
      (vertical ((0 1) (2 1)))))
    |}]
