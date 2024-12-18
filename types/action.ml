open! Core

type t = Start | Change | Swap
[@@deriving sexp_of, of_string ~capitalize:"UPPER SENTENCE CASE", sexp_of]

let%expect_test "Correctly parses the CLI command to an action" =
  let t_of_string = List.map [ "START"; "CHANGE"; "SWAP" ] ~f:of_string in
  print_s [%message (t_of_string : t list)];
  [%expect {| (t_of_string (Start Change Swap)) |}]
