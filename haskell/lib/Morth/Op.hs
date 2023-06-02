module Morth.Op (Op (..)) where

data Op
  = OpPush Int
  | OpPlus
  | OpMinus
  | OpEq
  | OpDump
  | OpIf Int
  | OpEnd
  deriving (Eq, Show)
