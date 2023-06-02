module Morth.Parser (parseProgram) where

import Control.Exception (throw)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Formatting (string, (%))
import Morth.Errors (ParseError (..))
import Morth.Lexer (lexFile)
import Morth.Logger (logErrLoc)
import Morth.Op (Op (..))
import Morth.Token (Token (..))
import Text.Read (readEither)

parseWord :: Token -> IO Op
parseWord token = case value token of
  "+" -> return OpPlus
  "-" -> return OpMinus
  "=" -> return OpEq
  ">" -> return OpGt
  "." -> return OpDump
  "if" -> return $ OpIf (-1)
  "else" -> return $ OpElse (-1)
  "end" -> return OpEnd
  "dup" -> return OpDup
  nt -> do
    n <- case readEither (T.unpack nt) of
      Right n -> return n
      Left err -> do
        logErrLoc (location token) ("Invalid number: " % string) err
        throw ParseError
    return $ OpPush n

parseProgram :: T.Text -> TL.Text -> IO [Op]
parseProgram fp s = mapM parseWord $ lexFile fp s
