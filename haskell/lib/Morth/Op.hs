module Morth.Op (OpCode (..), Op (..)) where

import qualified Data.Text.Lazy as TL
import Morth.Token (Location)

data OpCode
  = OpPushInt Int
  | OpPushStr TL.Text
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
