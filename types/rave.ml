open! Core

type t = { visits : int; reward : float }
[@@deriving bin_io, fields ~iterators:create ~getters]

let create = Fields.create
