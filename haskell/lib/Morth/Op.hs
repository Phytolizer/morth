module Morth.Op (OpCode (..), Op (..)) where

import Morth.Token (Location)

data OpCode
  = OpPush Int
  | OpDup
  | Op2Dup
  | OpMem
  | OpLoad
  | OpStore
  | OpSyscall0
  | OpSyscall1
  | OpSyscall2
  | OpSyscall3
  | OpSyscall4
  | OpSyscall5
  | OpSyscall6
  | OpPlus
  | OpMinus
  | OpEq
  | OpGt
  | OpLt
  | OpDump
  | OpIf Int
  | OpElse Int
  | OpWhile
  | OpDo Int
  | OpEnd Int
  deriving (Eq, Show)

data Op = Op
  { opCode :: OpCode
  , opLocation :: Location
  }
