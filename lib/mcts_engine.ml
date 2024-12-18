open! Core
open! Import

let random_elem lst = List.nth_exn lst (Random.int (List.length lst))

let available_moves_list state =
  state |> State.board |> Board.filter_available_moves |> Set.to_list

let update_state_with_move state move =
  let update_board =
    Board.make_move (State.board state) move (State.current_player state)
  in
  let updated_state_with_board = State.update_board state update_board in
  let updated_state_with_move =
    State.update_move_made updated_state_with_board move
  in
  State.switch_player updated_state_with_move

let combined_uct_rave ~n0 parent child c =
  let child_visits = float_of_int (Node.no_of_times_visited !child) in
  if Float.equal child_visits 0. then Float.infinity
  else
    let parent_visits = float_of_int (Node.no_of_times_visited !parent) in
    let q_uct = Node.reward !child /. child_visits in
    let move_made = State.move_made (Node.state !child) in
    let rave_visits, rave_reward =
      match Map.find (Node.rave_stats !parent) move_made with
      | None -> (0., 0.)
      | Some rv -> (float_of_int (Rave.visits rv), Rave.reward rv)
    in
    let q_rave =
      if Float.(rave_visits > 0.) then rave_reward /. rave_visits else 0.
    in
    let beta = n0 /. (child_visits +. n0) in
    let q_combined = ((1. -. beta) *. q_uct) +. (beta *. q_rave) in
    let exploration = c *. sqrt (log parent_visits /. child_visits) in
    q_combined +. exploration

let rec select_child_to_explore ?(path_history = []) ~c ~n0 node =
  match Node.children !node with
  | [] -> (node, path_history)
  | children ->
      let best_child =
        List.fold children ~init:(random_elem children)
          ~f:(fun best_child child ->
            if
              Float.(
                combined_uct_rave ~n0 node child c
                > combined_uct_rave ~n0 node best_child c)
            then child
            else best_child)
      in
      select_child_to_explore ~n0
        ~path_history:(best_child :: path_history)
        ~c best_child

let expand node =
  let children =
    let state = Node.state !node in
    let board = State.board state in
    state |> available_moves_list
    (* Prune the moves list using heuristics to not explore all possible moves *)
    |> List.filter ~f:(fun move -> not (Pattern.prune_move_1 board move))
    |> List.filter ~f:(fun move -> not (Pattern.prune_move_2 board move))
    |> List.map ~f:(fun move ->
           ref (Node.create ~state:(update_state_with_move state move)))
  in
  node := Node.update_children !node children;
  ()

let rec simulate state =
  match available_moves_list state with
  | [] ->
      let winner = Board.find_winner (State.board state) in
      ( Option.value_exn ~message:"Reached an unexpected state of a draw!" winner,
        [] )
  | moves ->
      let move = random_elem moves in
      let new_state = update_state_with_move state move in
      let winner, played_moves = simulate new_state in
      (winner, move :: played_moves)

let backpropogate simulations path_history =
  List.iter simulations ~f:(fun (leaf_node, winner_colour, played_moves) ->
      let won = Color.is_red winner_colour in
      let full_path_nodes = leaf_node :: path_history in
      List.iter full_path_nodes ~f:(fun node_ref ->
          let visited_node = Node.visit !node_ref in
          let updated_node =
            if won then Node.increment_reward visited_node else visited_node
          in
          let updated_node =
            List.fold played_moves ~init:updated_node ~f:(fun acc_node mv ->
                Node.update_rave_stats acc_node mv won)
          in
          node_ref := updated_node))

let mcts root pool ~n0 ~c ~n_simulations =
  let selected_child, path_history = select_child_to_explore ~n0 ~c root in
  expand selected_child;
  let simulations =
    let to_simulate =
      let children = Node.children !selected_child in
      List.concat (List.init n_simulations ~f:(Fn.const children))
    in
    List.map to_simulate ~f:(fun node ->
        Domainslib.Task.async pool (fun () ->
            let winner_color, played_moves = simulate (Node.state !node) in
            (node, winner_color, played_moves)))
    |> List.map ~f:(Domainslib.Task.await pool)
  in
  backpropogate simulations path_history

let run ?node ~board_size ~iterations ~n0 ~c ~cores ~n_simulations () =
  let root =
    Option.value_map node
      ~default:(ref (Node.create ~state:(State.empty ~board_size)))
      ~f:ref
  in
  (* Use multi-core processing to speed up MCTS execution *)
  let pool = Domainslib.Task.setup_pool ~num_domains:(cores - 1) () in
  let () =
    Domainslib.Task.run pool (fun () ->
        List.iter (List.init iterations ~f:Fn.const) ~f:(fun _ ->
            mcts root pool ~n0 ~c ~n_simulations))
  in
  Domainslib.Task.teardown_pool pool;
  !root
