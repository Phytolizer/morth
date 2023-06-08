module Morth.Op (
  Jump (..),
  OpCode (..),
  Op (..),
  Value (..),
  pushInt,
  pushStr,
  mapJmp,
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
  | OpMacro
  | OpEnd Jump
  deriving (Eq, Show)

mapJmp :: Jump -> Op -> Op
mapJmp j op =
  op
    { opCode = case opCode op of
        (OpIf _) -> OpIf j
        (OpElse _) -> OpElse j
        (OpDo _) -> OpDo j
        (OpEnd _) -> OpEnd j
        _ -> opCode op
    }

data Op = Op
  { opLocation :: Location
  , opCode :: OpCode
  }

pushInt :: Location -> Int -> Op
pushInt loc n = Op loc $ OpPush $ ValInt n

pushStr :: Location -> TL.Text -> Op
pushStr loc s = Op loc $ OpPush $ ValStr s
