open! Core

type t = { grid : Color.t option list list; size : int }
[@@deriving bin_io, equal, fields ~getters, sexp_of]

let at_cell t move =
  let x, y = Move.to_coordinates move in
  if x < 0 || y < 0 || x > t.size - 1 || y > t.size - 1 then None
  else List.nth_exn (List.nth_exn t.grid x) y

let empty size =
  let grid = List.init size ~f:(Fn.const (List.init size ~f:(Fn.const None))) in
  { grid; size }

let filter_moves t ~init_set ~f =
  List.foldi t.grid ~init:init_set ~f:(fun i set row ->
      List.foldi row ~init:set ~f:(fun j st elem ->
          if Option.is_none elem then Set.add st (f i j) else st))

let filter_available_moves =
  filter_moves ~init_set:Move.Set.empty ~f:(fun i j ->
      Move.of_coordinates (i, j))

let filter_available_moves_weighted =
  filter_moves ~init_set:Weighted_move.Set.empty ~f:(fun i j ->
      Weighted_move.create (Move.of_coordinates (i, j)))

let make_move t move color =
  let x, y = Move.to_coordinates move in
  let move_is_within_bounds = x >= 0 && x < t.size && y >= 0 && y < t.size in
  match (x, y) with
  | x', y' when move_is_within_bounds ->
      let updated_row =
        List.mapi (List.nth_exn t.grid x) ~f:(fun i cell ->
            if i = y' && Option.is_none cell then Some color else cell)
      in
      let grid =
        List.mapi t.grid ~f:(fun i row -> if i = x' then updated_row else row)
      in
      { t with grid }
  | (_ : int), (_ : int) -> t

let of_string size board =
  let grid =
    List.map (String.split board ~on:',') ~f:(fun row ->
        List.init size ~f:(fun i ->
            match row.[i] with
            | '0' -> None
            | color -> Some (Color.of_string (Char.to_string color))))
  in
  { grid; size }

let to_bitmap t player_color =
  let board =
    (* This is actually important, the presentation of blue is not symmetric
       in orientation of board. It must be presented in this way. *)
    if Color.is_blue player_color then
      List.transpose_exn t.grid |> List.rev |> List.map ~f:List.rev
    else t.grid
  in
  List.map board ~f:(fun row ->
      List.fold row ~init:0 ~f:(fun acc cell ->
          let bit =
            match cell with
            | Some color when Color.equal color player_color -> 1
            | (_ : Color.t option) -> 0
          in
          (acc lsl 1) + bit))

let union_find_winner_with_sets t colour =
  let neighbours = [ (-1, 0); (1, 0); (0, -1); (0, 1); (-1, 1); (1, -1) ] in
  (* Collect all cells belonging to this colour *)
  let colour_cells =
    List.concat_mapi t.grid ~f:(fun r row ->
        List.filter_mapi row ~f:(fun c cell ->
            match cell with
            | Some col when Color.equal col colour -> Some ((r * t.size) + c)
            | _ -> None))
  in
  (* Initially, each cell is its own singleton set *)
  let initial_components =
    List.map colour_cells ~f:(fun id -> Int.Set.singleton id)
  in
  let find_set components x = List.find components ~f:(fun s -> Set.mem s x) in
  let union_sets components a b =
    match (find_set components a, find_set components b) with
    | Some sa, Some sb when phys_equal sa sb -> components
    | Some sa, Some sb ->
        let merged = Set.union sa sb in
        merged
        :: List.filter components ~f:(fun s ->
               not (phys_equal s sa || phys_equal s sb))
    | _ -> components
  in
  (* Union all adjacent cells of the same colour *)
  let components =
    List.fold colour_cells ~init:initial_components ~f:(fun components id ->
        let r = id / t.size in
        let c = id mod t.size in
        List.fold neighbours ~init:components ~f:(fun components (dr, dc) ->
            let r' = r + dr in
            let c' = c + dc in
            if r' >= 0 && r' < t.size && c' >= 0 && c' < t.size then
              let neighbor_id = (r' * t.size) + c' in
              (* Check if neighbor is same colour *)
              match List.nth_exn (List.nth_exn t.grid r') c' with
              | Some col when Color.equal col colour ->
                  union_sets components id neighbor_id
              | _ -> components
            else components))
  in
  let iterable_over_size_of_grid = List.init t.size ~f:Fn.id in
  let index_in_grid_2d x y =
    List.nth t.grid x
    |> Option.map ~f:(fun grid -> List.nth grid y)
    |> Option.join |> Option.value_exn
  in
  (* Check winning condition *)
  match colour with
  | Color.Red ->
      (* Red: top-to-bottom *)
      let top_ids =
        List.filter iterable_over_size_of_grid ~f:(fun c ->
            match index_in_grid_2d 0 c with
            | Some col when Color.equal col Color.Red -> true
            | _ -> false)
      in
      let bottom_ids =
        List.filter iterable_over_size_of_grid ~f:(fun c ->
            match index_in_grid_2d (t.size - 1) c with
            | Some col when Color.equal col Color.Red -> true
            | _ -> false)
        |> List.map ~f:(fun c -> ((t.size - 1) * t.size) + c)
      in
      List.exists top_ids ~f:(fun tid ->
          match find_set components tid with
          | None -> false
          | Some st -> List.exists bottom_ids ~f:(fun bid -> Set.mem st bid))
  | Blue ->
      (* Blue: left-to-right *)
      let left_ids =
        List.filter iterable_over_size_of_grid ~f:(fun r ->
            match index_in_grid_2d r 0 with
            | Some col when Color.equal col Color.Blue -> true
            | _ -> false)
        |> List.map ~f:(fun r -> r * t.size)
      in
      let right_ids =
        List.filter iterable_over_size_of_grid ~f:(fun r ->
            match index_in_grid_2d r (t.size - 1) with
            | Some col when Color.equal col Color.Blue -> true
            | _ -> false)
        |> List.map ~f:(fun r -> (r * t.size) + (t.size - 1))
      in
      List.exists left_ids ~f:(fun lid ->
          match find_set components lid with
          | None -> false
          | Some st -> List.exists right_ids ~f:(fun rid -> Set.mem st rid))

let colour_has_won t colour =
  let bitmap = to_bitmap t colour in
  let initial_sweeping_line = (1 lsl t.size) - 1 in
  let rec process_rows sweeping_line shifted_sweeping_line = function
    | [] -> sweeping_line <> 0
    | row :: rest ->
        let rec process_bits current next_sweeping_line pointer =
          if pointer > row * 2 then next_sweeping_line
          else
            let current, next_sweeping_line =
              if row land pointer = 0 then
                if
                  current <> 0
                  && (current land sweeping_line <> 0
                     || current land shifted_sweeping_line <> 0)
                then (0, next_sweeping_line lor current)
                else (0, next_sweeping_line)
              else (current lor pointer, next_sweeping_line)
            in
            process_bits current next_sweeping_line (pointer lsl 1)
        in
        let next_sweeping_line = process_bits 0 0 1 in
        if next_sweeping_line = 0 then false
        else
          let next_shifted_sweeping_line = next_sweeping_line lsl 1 in
          process_rows next_sweeping_line next_shifted_sweeping_line rest
  in
  process_rows initial_sweeping_line initial_sweeping_line bitmap

let find_winner t =
  match (colour_has_won t Red, colour_has_won t Blue) with
  | true, false -> Some Color.Red
  | false, true -> Some Blue
  | (_ : bool), (_ : bool) ->
      (* If bitmap solution fails, fallback to set-based union-find approach *)
      List.fold Color.all ~init:None ~f:(fun opt_res color ->
          if union_find_winner_with_sets t color then Some color else opt_res)

module Board_test = struct
  let%expect_test "Create a simple 3x3 board from string" =
    let t_of_string = of_string 3 "000,0RB,RB0" in
    print_s [%message (t_of_string : t)];
    [%expect
      {|
      (t_of_string
       ((grid ((() () ()) (() (Red) (Blue)) ((Red) (Blue) ()))) (size 3)))
      |}]

  let%expect_test "Create an empty 11x11 board" =
    let empty_board = empty 11 in
    print_s [%message (empty_board : t)];
    [%expect
      {|
      (empty_board
       ((grid
         ((() () () () () () () () () () ()) (() () () () () () () () () () ())
          (() () () () () () () () () () ()) (() () () () () () () () () () ())
          (() () () () () () () () () () ()) (() () () () () () () () () () ())
          (() () () () () () () () () () ()) (() () () () () () () () () () ())
          (() () () () () () () () () () ()) (() () () () () () () () () () ())
          (() () () () () () () () () () ())))
        (size 11)))
      |}]

  let%expect_test "Available moves sorted by weight on a partially filled 3x3 \
                   board" =
    let t_of_string = of_string 3 "000,0RB,RB0" in
    let available_moves = filter_available_moves_weighted t_of_string in
    print_s [%message (available_moves : Weighted_move.Set.t)];
    [%expect
      {|
      (available_moves
       ((0.013139006488339289 (0 0)) (0.028656376350145975 (0 1))
        (0.028656376350145975 (1 0)) (0.052556025953357156 (0 2))
        (0.21022410381342863 (2 2))))
      |}]

  let%expect_test "Make a move on cell 0,0" =
    let t_of_string = of_string 3 "000,0RB,RB0" in
    let board = make_move t_of_string (Move.of_coordinates (0, 0)) Blue in
    print_s [%message (board : t)];
    [%expect
      {|
      (board
       ((grid (((Blue) () ()) (() (Red) (Blue)) ((Red) (Blue) ()))) (size 3)))
      |}]

  let%expect_test "Convert a 5x5 board to bitmap for Red and Blue" =
    let int_to_binary_string len n =
      (* Helper functions for printing binary bitmaps *)
      let bin_str =
        Int.Binary.to_string n
        |> String.substr_replace_all ~pattern:"0b" ~with_:""
      in
      String.pad_left bin_str ~len ~char:'0'
    in
    let board = of_string 5 "R0BR0,0RR00,00BRB,BR00B,00R0R" in
    let bitmap_red =
      to_bitmap board Red
      |> List.map ~f:(int_to_binary_string 5)
      |> String.concat ~sep:"\n"
    in
    let bitmap_blue =
      to_bitmap board Blue
      |> List.map ~f:(int_to_binary_string 5)
      |> String.concat ~sep:"\n"
    in
    print_endline bitmap_red;
    print_endline "";
    print_endline bitmap_blue;
    [%expect
      {|
      10010
      01100
      00010
      01000
      00101

      01100
      00000
      00101
      00000
      01000
      |}]

  let%test "Does not overwrite an occupied cell" =
    let t_of_string = of_string 3 "000,0RB,RB0" in
    equal (make_move t_of_string (Move.of_coordinates (1, 1)) Blue) t_of_string

  let%test "3x3 board: No winner" =
    let board = of_string 3 "0RR,B0R,0BB" in
    Bool.equal (Option.is_none (find_winner board)) true

  let%test "5x5 board: Blue wins (complex snake path)" =
    let board = of_string 5 "BBBBR,RRRBR,RBBRR,RBBBB,RRBRR" in
    Option.value_map (find_winner board) ~default:false ~f:(Color.equal Blue)

  let%test "11x11 board: Red wins with a simple vertical path" =
    let size = 11 in
    let rows =
      List.init size ~f:(fun _ ->
          String.init size ~f:(fun c -> if c = 0 then 'R' else '0'))
      |> String.concat ~sep:","
    in
    let board = of_string size rows in
    Option.value_map (find_winner board) ~default:false ~f:(Color.equal Red)

  let%test "11x11 board: Blue wins by occupying a full row" =
    let size = 11 in
    let rows =
      List.init size ~f:(fun r ->
          if r = size - 1 then String.init size ~f:(Fn.const 'B')
          else String.init size ~f:(Fn.const '0'))
      |> String.concat ~sep:","
    in
    let board = of_string size rows in
    Option.value_map (find_winner board) ~default:false ~f:(Color.equal Blue)

  let%test "Red winner on 3x3" =
    let winner = find_winner (of_string 3 "RBR,BRB,RBR") in
    Option.value_map winner ~default:false ~f:(Color.equal Red)

  let%test "Blue winner on 5x5" =
    let winner = find_winner (of_string 5 "BRBBB,BBRRB,RRBRB,RRRRR,BRRBB") in
    Option.value_map winner ~default:false ~f:(Color.equal Blue)

  let%test "Red winner on 3x3" =
    let winner = find_winner (of_string 3 "RRR,BRR,RBB") in
    Option.value_map winner ~default:false ~f:(Color.equal Red)

  let%test "Blue winner on a previously known tricky 5x5 snake case" =
    let winner = find_winner (of_string 5 "BBBBR,RRRBR,RBBRR,RBBBB,RRBRR") in
    Option.value_map winner ~default:false ~f:(Color.equal Blue)
end
