module MorthLanguage.Parser (parseProgram) where

import MorthLanguage.Op (Op (..))

parseWord :: String -> Op
parseWord "+" = OpPlus
parseWord "-" = OpMinus
parseWord "." = OpDump
parseWord n = OpPush (read n)

parseProgram :: String -> [Op]
parseProgram = map parseWord . words
