open! Core
open! Import

val run :
  ?node:Node.t ->
  board_size:int ->
  iterations:int ->
  n0:float ->
  c:float ->
  cores:int ->
  n_simulations:int ->
  unit ->
  Node.t
