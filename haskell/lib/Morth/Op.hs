module Morth.Op (Op (..)) where

data Op
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
