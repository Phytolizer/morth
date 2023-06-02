module Morth.Op (OpCode (..), Op (..)) where

import Morth.Token (Location)

data OpCode
  = OpPush Int
  | OpDup
  | OpPlus
  | OpMinus
  | OpEq
  | OpGt
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
