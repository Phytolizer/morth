module Morth.Op (Op (..)) where

data Op
  = OpPush Int
  | OpDup
  | OpPlus
  | OpMinus
  | OpEq
  | OpDump
  | OpIf Int
  | OpElse Int
  | OpEnd
  deriving (Eq, Show)
