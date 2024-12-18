open! Core
open! Async
open! Import

let minified_default_loc =
  [%string "%{Filename.temp_dir_name}/mcts-game-tree-minified.bin"]

let iterations =
  Command.Param.(
    flag "iterations" (required int)
      ~doc:"INT The number of times to perform MCTS")

let c =
  Command.Param.(
    flag "c"
      (optional_with_default (sqrt 2.) float)
      ~doc:
        "FLOAT The exploration constant to control balance between exploration \
         and exploitation")

let cores =
  Command.Param.(
    flag "cores"
      (optional_with_default 1 int)
      ~doc:"INT The number of cores to use for running the MCTS engine")

let n_simulations =
  Command.Param.(
    flag "sims"
      (optional_with_default 50 int)
      ~doc:"INT The number of times to perform MCTS simulation per node")

let n0 =
  Command.Param.(
    flag "n0"
      (optional_with_default 3000. float)
      ~doc:"FLOAT The rave exploration constant")

let create_agent =
  Command.async_or_error ~summary:"Create a new agent player"
    [%map_open.Command
      let color =
        flag "color"
          (required (Arg_type.create Color.of_string))
          ~doc:"CHAR The game color assigned to the player"
      and board_size =
        flag "board-size"
          (optional_with_default 11 int)
          ~doc:"INT The number of rows and columns in the game board"
      and policy =
        flag "policy"
          (optional_with_default Move_policy.Max_child
             (Arg_type.create Move_policy.of_string))
          ~doc:"MOVE_POLICY The policy to play each move by"
      and iterations = iterations
      and c = c
      and n0 = n0
      and cores = cores
      and n_simulations = n_simulations in
      fun () ->
        let policy =
          if Color.is_blue color then Move_policy.Min_child else policy
        in
        Agent.run
          (Agent.create ~board_size ~color ~policy)
          ~iterations ~c ~cores ~n_simulations ~n0]

let mcts =
  Command.async_or_error ~summary:"Perform MCTS for Hex"
    [%map_open.Command
      let board_size =
        flag "board-size"
          (optional_with_default 11 int)
          ~doc:"INT The number of rows and columns in the game board"
      and iterations = iterations
      and c = c
      and cores = cores
      and n_simulations = n_simulations
      and n0 = n0
      and save_loc_filename =
        let default =
          [%string "%{Filename.temp_dir_name}/mcts-game-tree.bin"]
        in
        flag "save-to"
          (optional_with_default default Filename_unix.arg_type)
          ~doc:"FILENAME_UNIX The path of the file to save the node to"
      and load_loc_filename =
        flag "load-from"
          (optional Filename_unix.arg_type)
          ~doc:"FILENAME_UNIX The path of the file to load the node from"
      and minify =
        flag "minify"
          (optional_with_default false bool)
          ~doc:"BOOL Convert the node into a minified version"
      in
      fun () ->
        let%bind.Deferred.Or_error node =
          match load_loc_filename with
          | Some filename ->
              Reader.load_bin_prot filename Node.bin_reader_t
              |> Deferred.Or_error.map ~f:Option.some
          | None -> Deferred.Or_error.return None
        in
        let node =
          Mcts_engine.run ?node ~board_size ~iterations ~c ~n0
            ~cores:(if cores < 1 then 1 else cores)
            ~n_simulations ()
        in
        Deferred.Or_error.try_with (fun () ->
            if minify then
              Writer.save_bin_prot minified_default_loc
                Node.Minified.bin_writer_t
                (Node.to_minified ~n0 node)
            else Writer.save_bin_prot save_loc_filename Node.bin_writer_t node)]

let minify_node =
  Command.async_or_error
    ~summary:"Convert a serialised node into a serialised minified node"
    [%map_open.Command
      let save_loc_filename =
        flag "save-to"
          (optional_with_default minified_default_loc Filename_unix.arg_type)
          ~doc:"FILENAME_UNIX The path of the file to save the node to"
      and load_loc_filename =
        flag "load-from"
          (required Filename_unix.arg_type)
          ~doc:"FILENAME_UNIX The path of the file to load the node from"
      and n0 = n0 in
      fun () ->
        let%bind.Deferred.Or_error node =
          Reader.load_bin_prot load_loc_filename Node.bin_reader_t
        in
        Deferred.Or_error.try_with (fun () ->
            Writer.save_bin_prot save_loc_filename Node.Minified.bin_writer_t
              (Node.to_minified ~n0 node))]

let tree_stats =
  Command.async_or_error
    ~summary:"Get statistics about a Node.Minified.t game tree"
    [%map_open.Command
      let load_loc_filename =
        flag "load-from"
          (required Filename_unix.arg_type)
          ~doc:"FILENAME_UNIX The path of the file to load the node from"
      and policy =
        flag "policy"
          (optional_with_default Move_policy.Max_child
             (Arg_type.create Move_policy.of_string))
          ~doc:"MOVE_POLICY The policy to use to select children"
      in
      fun () ->
        let%map.Deferred.Or_error node =
          Reader.load_bin_prot load_loc_filename Node.Minified.bin_reader_t
        in
        (* Compute max depth *)
        let rec max_depth ?(depth = 0) node =
          match Node.Minified.children node with
          | [] -> depth
          | children ->
              List.fold children ~init:depth ~f:(fun acc child ->
                  Int.max (max_depth ~depth:(depth + 1) !child) acc)
        in
        (* Find a path according to the given policy *)
        let rec best_path ?(path = []) node_ref =
          match Move_policy.find_branch policy node_ref with
          | None -> List.map ~f:Node.Minified.move_made (List.rev path)
          | Some next_child_ref ->
              best_path ~path:(!next_child_ref :: path) next_child_ref
        in
        (* Compute total nodes and total children for average branching factor *)
        let rec tree_stats node =
          let children = Node.Minified.children node in
          let child_count = List.length children in
          let total_nodes, total_children =
            List.fold children ~init:(1, child_count)
              ~f:(fun (acc_nodes, acc_children) c ->
                let child_nodes, child_children = tree_stats !c in
                (acc_nodes + child_nodes, acc_children + child_children))
          in
          (total_nodes, total_children)
        in
        let max_depth = max_depth node in
        let path_based_on_policy = best_path (ref node) in
        let total_nodes, total_children = tree_stats node in
        let average_branching_factor =
          if total_nodes > 0 then float total_children /. float total_nodes
          else 0.
        in
        print_s
          [%message
            (max_depth : int)
              (path_based_on_policy : Move.t list)
              (total_nodes : int)
              (total_children : int)
              (average_branching_factor : float)]]

let command =
  Command.group ~summary:"Commander For The Hex Game"
    [
      ("create", create_agent);
      ("mcts", mcts);
      ("minify", minify_node);
      ("stats", tree_stats);
    ]
