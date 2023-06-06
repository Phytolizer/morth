module Morth.Op (
  Jump (..),
  OpCode (..),
  Op (..),
  Value (..),
  pushInt,
  pushStr,
) where

import qualified Data.Text.Lazy as TL
import Morth.Location (Location)

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

pushInt :: Int -> Location -> Op
pushInt n = Op $ OpPush $ ValInt n

pushStr :: TL.Text -> Location -> Op
pushStr s = Op $ OpPush $ ValStr s
