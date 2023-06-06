module Morth.Frontend (readProgram) where

import Control.Monad.Primitive (PrimMonad (PrimState))
import Data.Primitive (Array, MutableArray, arrayFromList, unsafeThawArray)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Morth.Blocks (resolveBlocks)
import Morth.Lexer (lexFile)
import Morth.Op (Op)
import Morth.Parser (parseProgram)

mutableArrayFromList :: [a] -> IO (MutableArray (PrimState IO) a)
mutableArrayFromList = unsafeThawArray . arrayFromList

readProgram :: T.Text -> TL.Text -> IO (Array Op)
readProgram fp text =
  lexFile fp text
    >>= parseProgram
    >>= mutableArrayFromList
    >>= resolveBlocks
