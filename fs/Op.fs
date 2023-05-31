module Morth.Op

type t =
  | Push of int
  | Plus
  | Minus
  | Eq
  | Dump
  | If of int
  | Else of int
  | End
