open! Core

module T = struct
  type t = int * int [@@deriving bin_io, compare, sexp]
end

let of_coordinates = Fn.id

let of_string str =
  let coords = String.split str ~on:',' |> List.map ~f:int_of_string in
  (List.nth_exn coords 0, List.nth_exn coords 1)

let to_coordinates = Fn.id
let to_string t = [%string "%{(fst t)#Int},%{(snd t)#Int}"]

let%expect_test "Converts a co-ordinate into the correct stdout format" =
  let t_to_string =
    to_string (of_coordinates (-1, -1))
    (* Swap move coordinates *)
  in
  print_s [%message (t_to_string : string)];
  [%expect {| (t_to_string -1,-1) |}]

include T
include Comparable.Make_binable (T)
