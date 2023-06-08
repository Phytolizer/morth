module Morth.Frontend (readProgram) where

import Data.Primitive (Array)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Morth.Lexer (lexFile)
import Morth.Op (Op)
import Morth.Parser (parseProgram)

readProgram :: T.Text -> TL.Text -> IO (Array Op)
readProgram fp text =
  lexFile fp text
    >>= parseProgram
