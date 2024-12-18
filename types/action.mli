open! Core

type t = Start | Change | Swap [@@deriving of_string]
