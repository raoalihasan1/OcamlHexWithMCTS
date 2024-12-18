open! Core

module Minified = struct
  type t = {
    move_made : Move.t;
    average_reward : float;
    no_of_times_visited : int;
    children : t ref list;
    rave_stats : Rave.t Move.Map.t;
  }
  [@@deriving bin_io, fields ~iterators:create ~getters]

  let create = Fields.create
end

type t = {
  state : State.t;
  no_of_times_visited : int;
  reward : float;
  children : t ref list;
  rave_stats : Rave.t Move.Map.t;
}
[@@deriving bin_io, fields ~iterators:create ~getters]

let create =
  Fields.create ~no_of_times_visited:0 ~reward:0. ~children:[]
    ~rave_stats:Move.Map.empty

let increment_reward t = { t with reward = t.reward +. 1. }

let rec to_minified t ~n0 =
  let visits = float_of_int t.no_of_times_visited in
  let q_uct = if t.no_of_times_visited = 0 then 0. else t.reward /. visits in
  let move_made = State.move_made t.state in
  let q_rave =
    match Map.find t.rave_stats move_made with
    | None -> 0.
    | Some rv ->
        let rave_visits = float_of_int (Rave.visits rv) in
        if Float.equal rave_visits 0. then 0. else Rave.reward rv /. rave_visits
  in
  let beta = n0 /. (visits +. n0) in
  let combined_average = ((1. -. beta) *. q_uct) +. (beta *. q_rave) in
  Minified.create ~move_made ~average_reward:combined_average
    ~no_of_times_visited:t.no_of_times_visited
    ~children:
      (List.map t.children ~f:(fun child -> ref (to_minified ~n0 !child)))
    ~rave_stats:t.rave_stats

let update_children t children = { t with children }

let update_rave_stats t move won =
  let old_rave =
    match Map.find t.rave_stats move with
    | None -> Rave.create ~visits:0 ~reward:0.
    | Some rv -> rv
  in
  let updated_rave =
    Rave.create
      ~visits:(Rave.visits old_rave + 1)
      ~reward:(Rave.reward old_rave +. if won then 1. else 0.)
  in
  { t with rave_stats = Map.set t.rave_stats ~key:move ~data:updated_rave }

let update_state t state = { t with state }
let visit t = { t with no_of_times_visited = t.no_of_times_visited + 1 }
