module Morth.Language.Op

type code =
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
  | Mem
  | Load
  | Store
  | Syscall1
  | Syscall2
  | Syscall3
  | Syscall4
  | Syscall5
  | Syscall6

type t =
  struct
    val loc : Token.loc
    val mutable code : code

    new(loc : Token.loc, code : code) = { loc = loc; code = code }
  end
