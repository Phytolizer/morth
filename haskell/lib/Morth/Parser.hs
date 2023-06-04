module Morth.Parser (parseProgram) where

import Control.Exception (throw)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Formatting (string, (%))
import Morth.Errors (MorthError (ParseError))
import Morth.Lexer (lexFile)
import Morth.Logger (logErrLoc)
import Morth.Op (Op (..), OpCode (..))
import Morth.Token (Token (..))
import Text.Read (readEither)

parseWord :: Token -> IO Op
parseWord token =
  let loc = location token
   in case value token of
        "mem" -> return $ Op OpMem loc
        "." -> return $ Op OpStore loc
        "," -> return $ Op OpLoad loc
        "syscall0" -> return $ Op OpSyscall0 loc
        "syscall1" -> return $ Op OpSyscall1 loc
        "syscall2" -> return $ Op OpSyscall2 loc
        "syscall3" -> return $ Op OpSyscall3 loc
        "syscall4" -> return $ Op OpSyscall4 loc
        "syscall5" -> return $ Op OpSyscall5 loc
        "syscall6" -> return $ Op OpSyscall6 loc
        "+" -> return $ Op OpPlus loc
        "-" -> return $ Op OpMinus loc
        "=" -> return $ Op OpEq loc
        ">" -> return $ Op OpGt loc
        "<" -> return $ Op OpLt loc
        "dump" -> return $ Op OpDump loc
        "if" -> return $ Op (OpIf (-1)) loc
        "else" -> return $ Op (OpElse (-1)) loc
        "while" -> return $ Op OpWhile loc
        "do" -> return $ Op (OpDo (-1)) loc
        "end" -> return $ Op (OpEnd (-1)) loc
        "dup" -> return $ Op OpDup loc
        "2dup" -> return $ Op Op2Dup loc
        "swap" -> return $ Op OpSwap loc
        ntext -> do
          n <- case readEither (T.unpack ntext) of
            Right n -> return n
            Left err -> do
              logErrLoc loc ("Invalid number: " % string) err
              throw ParseError
          return $ Op (OpPush n) loc

parseProgram :: T.Text -> TL.Text -> IO [Op]
parseProgram fp s = mapM parseWord $ lexFile fp s
