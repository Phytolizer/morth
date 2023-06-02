module MorthLanguage.Op (Op (..)) where

data Op
  = OpPush Int
  | OpPlus
  | OpMinus
  | OpEq
  | OpDump
  deriving (Eq, Show)
