open! Core

let moves' = function
  | `Diagonal -> Neighbours.diagonal
  | `Horizontal -> Neighbours.horizontal
  | `Vertical -> Neighbours.vertical

let prune_move_1 board move =
  let sum board plane moves =
    let moves = moves' plane moves in
    if List.is_empty moves then Float.infinity
    else
      List.fold moves ~init:0. ~f:(fun acc move' ->
          match Board.at_cell board move' with
          | Some color -> acc +. float_of_int (Color.to_weight color)
          | None -> Float.infinity)
  in
  let neighbours = Neighbours.of_move move ~board_size:(Board.size board) in
  let dead_cell_patterns =
    [
      [ `Horizontal; `Diagonal ];
      [ `Vertical; `Diagonal ];
      [ `Vertical; `Horizontal ];
    ]
  in
  List.exists dead_cell_patterns ~f:(fun pattern ->
      let is_dead_pattern =
        List.for_all pattern ~f:(fun plane ->
            Float.equal (sum board plane neighbours) 0.)
      in
      is_dead_pattern)

let prune_move_2 board move =
  let board_size = Board.size board in
  let dead_cell_prune_patterns =
    [
      [ (-1, 0); (0, -1); (1, -1); (1, 0) ];
      [ (0, -1); (1, -1); (1, 0); (0, 1) ];
      [ (1, -1); (1, 0); (0, 1); (-1, 1) ];
      [ (1, 0); (0, 1); (-1, 1); (-1, 0) ];
      [ (0, 1); (-1, 1); (-1, 0); (0, -1) ];
      [ (-1, 1); (-1, 0); (0, -1); (1, -1) ];
    ]
  in
  let all_same lst =
    match lst with [] -> true | x :: xs -> List.for_all ~f:(Color.equal x) xs
  in
  let is_within_bounds x y =
    x >= 0 && x < board_size && y >= 0 && y < board_size
  in
  let get_neighbour_cell dx dy =
    let x, y = Move.to_coordinates move in
    let nx, ny = (x + dx, y + dy) in
    if is_within_bounds nx ny then
      Board.at_cell board (Move.of_coordinates (nx, ny))
    else None
  in
  match Board.at_cell board move with
  | None ->
      List.exists dead_cell_prune_patterns ~f:(fun pattern ->
          let filtered_cells =
            List.filter_map pattern ~f:(fun (dx, dy) ->
                get_neighbour_cell dx dy)
          in
          all_same filtered_cells
          && List.length filtered_cells = List.length pattern)
  | Some (_ : Color.t) -> false
