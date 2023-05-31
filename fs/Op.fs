module Morth.Op

type t =
  | Push of int
  | Dup
  | Plus
  | Minus
  | Eq
  | Gt
  | Dump
  | If of int
  | Else of int
  | While
  | Do of int
  | End of int
