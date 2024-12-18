open! Core
open! Async
open! Import

type t

val create : color:Color.t -> board_size:int -> policy:Move_policy.t -> t

val run :
  t ->
  iterations:int ->
  n0:float ->
  c:float ->
  cores:int ->
  n_simulations:int ->
  unit Or_error.t Deferred.t
