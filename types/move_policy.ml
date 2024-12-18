open! Core

type t = Min_child | Max_child | Robust_child | Max_robust_child
[@@deriving of_string ~capitalize:"kebab-case"]

let find_branch t node =
  let children = Node.Minified.children !node in
  let find_child ~f =
    List.fold children ~init:(List.hd children) ~f:(fun best_child child ->
        Option.map best_child ~f:(fun best_child ->
            if f !best_child !child then child else best_child))
  in
  let min_max_child child child' ~f =
    f (Node.Minified.average_reward child) (Node.Minified.average_reward child')
  in
  match t with
  | Max_child ->
      find_child ~f:(fun best_child child ->
          min_max_child child best_child ~f:Float.( > ))
  | Min_child ->
      find_child ~f:(fun best_child child ->
          min_max_child child best_child ~f:Float.( < ))
  | Robust_child ->
      find_child ~f:(fun best_child child ->
          Node.Minified.no_of_times_visited child
          > Node.Minified.no_of_times_visited best_child)
  | Max_robust_child ->
      find_child ~f:(fun best_child child ->
          Node.Minified.no_of_times_visited child
          > Node.Minified.no_of_times_visited best_child
          && Float.(
               Node.Minified.average_reward child
               > Node.Minified.average_reward best_child))
