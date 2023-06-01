module MorthLanguage.Op (Op (..)) where

data Op = OpPush Int | OpPlus | OpMinus | OpDump
  deriving (Eq, Show)
