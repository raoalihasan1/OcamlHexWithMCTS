open! Core

type t = Red | Blue [@@deriving bin_io, enumerate, equal, sexp_of, variants]

let of_string str =
  match String.lowercase str with
  | "r" -> Red
  | "b" -> Blue
  | char -> failwith [%string "Invalid character: %{char}"]

let opponent = function Red -> Blue | Blue -> Red
let to_weight = function Red -> 1 | Blue -> -1

let%expect_test "Correctly parses the CLI color character" =
  let t_of_string = List.map [ "b"; "r"; "B"; "R" ] ~f:of_string in
  print_s [%message (t_of_string : t list)];
  [%expect {| (t_of_string (Blue Red Blue Red)) |}]
