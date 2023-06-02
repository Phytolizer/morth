module MorthLanguage.Parser (parseProgram) where

import Control.Exception (Exception, throw)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Formatting (string, (%))
import MorthLanguage.Lexer (lexFile)
import MorthLanguage.Logger (logErrLoc)
import MorthLanguage.Op (Op (..))
import MorthLanguage.Token (Token (..))
import Text.Read (readEither)

data ParseError = ParseError deriving (Show)

instance Exception ParseError

parseWord :: Token -> IO Op
parseWord token = case value token of
  "+" -> return OpPlus
  "-" -> return OpMinus
  "." -> return OpDump
  nt -> do
    n <- case readEither (T.unpack nt) of
      Right n -> return n
      Left err -> do
        logErrLoc (location token) ("Invalid number: " % string) err
        throw ParseError
    return $ OpPush n

parseProgram :: T.Text -> TL.Text -> IO [Op]
parseProgram fp s = mapM parseWord $ lexFile fp s
