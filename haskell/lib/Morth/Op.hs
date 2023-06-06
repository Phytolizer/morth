module Morth.Op (Jump (..), OpCode (..), Op (..), Value (..)) where

import qualified Data.Text.Lazy as TL
import Morth.Token (Location)

data Value
  = ValInt Int
  | ValStr TL.Text
  deriving (Eq, Show)

data Jump
  = JumpNil
  | JumpTo Int
  deriving (Eq, Show)

data OpCode
  = OpPush Value
  | OpDup
  | Op2Dup
  | OpSwap
  | OpDrop
  | OpOver
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
  | OpMod
  | OpEq
  | OpNe
  | OpGt
  | OpLt
  | OpGe
  | OpLe
  | OpShl
  | OpShr
  | OpBand
  | OpBor
  | OpPrint
  | OpIf Jump
  | OpElse Jump
  | OpWhile
  | OpDo Jump
  | OpEnd Jump
  deriving (Eq, Show)

data Op = Op
  { opCode :: OpCode
  , opLocation :: Location
  }
