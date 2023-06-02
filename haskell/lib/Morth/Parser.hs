module Morth.Parser (parseProgram) where

import Control.Exception (throw)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Formatting (string, (%))
import Morth.Errors (ParseError (..))
import Morth.Lexer (lexFile)
import Morth.Logger (logErrLoc)
import Morth.Op (Op (..), OpCode (..))
import Morth.Token (Token (..))
import Text.Read (readEither)

parseWord :: Token -> IO Op
parseWord token =
  let loc = location token
   in case value token of
        "+" -> return $ Op OpPlus loc
        "-" -> return $ Op OpMinus loc
        "=" -> return $ Op OpEq loc
        ">" -> return $ Op OpGt loc
        "." -> return $ Op OpDump loc
        "if" -> return $ Op (OpIf (-1)) loc
        "else" -> return $ Op (OpElse (-1)) loc
        "while" -> return $ Op OpWhile loc
        "do" -> return $ Op (OpDo (-1)) loc
        "end" -> return $ Op (OpEnd (-1)) loc
        "dup" -> return $ Op OpDup loc
        ntext -> do
          n <- case readEither (T.unpack ntext) of
            Right n -> return n
            Left err -> do
              logErrLoc loc ("Invalid number: " % string) err
              throw ParseError
          return $ Op (OpPush n) loc

parseProgram :: T.Text -> TL.Text -> IO [Op]
parseProgram fp s = mapM parseWord $ lexFile fp s
