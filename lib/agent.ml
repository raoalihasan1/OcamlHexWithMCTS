open! Core
open! Async
open! Import

type t = {
  board_size : int;
  mutable turn : int;
  mutable color : Color.t;
  mutable policy : Move_policy.t;
}

let create ~color ~board_size ~policy =
  { board_size; turn = (if Color.is_red color then 0 else 1); color; policy }

let create_action_move_board t str =
  let message =
    str |> String.chop_suffix_if_exists ~suffix:"\n" |> String.split ~on:';'
  in
  match List.hd message with
  | Some action -> (
      match List.nth message 2 with
      | Some board ->
          t.turn <- t.turn + 1;
          Deferred.Or_error.return
            (Action.of_string action, Board.of_string t.board_size board)
      | None ->
          Deferred.Or_error.error_string "Failed to parse the board coordinates"
      )
  | None -> Deferred.Or_error.error_string "Failed to determine the instruction"

let find_weighted_move board =
  board |> Board.filter_available_moves_weighted |> Set.to_list |> List.hd
  |> function
  | Some weighted_move -> Weighted_move.move weighted_move
  | None -> failwith "Board is full!"

let move_to_make t move =
  let play_swap_rule =
    Color.is_blue t.color && t.turn = 2
    && Set.mem Constants.swap_moves_11_by_11 move
  in
  (* Always swap for board size less than 11 as we haven't added those swap moves yet. *)
  if play_swap_rule || t.board_size < 11 then Move.of_coordinates (-1, -1)
  else if t.turn = 1 then
    (* Use one of these known strong starting positions for the game rather than MCTS *)
    let start_moves =
      [
        (5, 5);
        (6, 5);
        (5, 6);
        (5, 4);
        (4, 5);
        (2, 2);
        (9, 2);
        (3, 7);
        (8, 8);
        (1, 8);
      ]
    in
    Move.of_coordinates
      (List.nth_exn start_moves (Random.int (List.length start_moves)))
  else move

let perform_action t action move =
  let print_move = print_endline (Move.to_string move) in
  match action with
  | Action.Start when Color.is_red t.color -> print_move
  | Change -> print_move
  | Swap ->
      t.color <- Color.opponent t.color;
      t.policy <-
        (if Color.is_red t.color then Move_policy.Max_child else Min_child);
      print_move
  | (_ : Action.t) -> ()

let rec run t ~iterations ~n0 ~c ~cores ~n_simulations =
  let recurse_move action move' =
    let move = move_to_make t move' in
    perform_action t action move;
    run t ~iterations ~n0 ~c ~cores ~n_simulations:(n_simulations + 5)
  in
  match%bind Reader.read_line (Lazy.force Reader.stdin) with
  | `Ok message -> (
      let open Deferred.Or_error.Let_syntax in
      create_action_move_board t message >>= fun (action, board) ->
      let node =
        let state =
          State.create ~board ~current_player:t.color
            ~move_made:(Move.of_coordinates (-1, -1))
        in
        Mcts_engine.run ~node:(Node.create ~state) ~board_size:t.board_size
          ~iterations ~c ~cores ~n_simulations ~n0 ()
        |> Node.to_minified ~n0 |> ref
      in
      match Move_policy.find_branch t.policy node with
      | None -> recurse_move action (find_weighted_move board)
      | Some child_node ->
          recurse_move action (Node.Minified.move_made !child_node))
  | `Eof -> Deferred.Or_error.error_string "Failed to establish I/O connection"
