module Morth.Op (Op (..)) where

data Op
  = OpPush Int
  | OpPlus
  | OpMinus
  | OpEq
  | OpDump
  | OpIf Int
  | OpElse Int
  | OpEnd
  deriving (Eq, Show)
