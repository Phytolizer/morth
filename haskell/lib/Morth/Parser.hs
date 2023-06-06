module Morth.Parser (parseProgram) where

import Control.Exception (throw)
import qualified Data.Text.Lazy as TL
import Formatting (text, (%))
import Morth.Errors (MorthError (ParseError))
import Morth.Logger (logErrLoc)
import Morth.Op (
  Jump (JumpNil),
  Op (..),
  OpCode (..),
  pushInt,
  pushStr,
 )
import Morth.Token (Token (..), TokenKind (..))

builtinWords :: TL.Text -> Maybe OpCode
builtinWords "mem" = Just OpMem
builtinWords "." = Just OpStore
builtinWords "," = Just OpLoad
builtinWords "syscall0" = Just OpSyscall0
builtinWords "syscall1" = Just OpSyscall1
builtinWords "syscall2" = Just OpSyscall2
builtinWords "syscall3" = Just OpSyscall3
builtinWords "syscall4" = Just OpSyscall4
builtinWords "syscall5" = Just OpSyscall5
builtinWords "syscall6" = Just OpSyscall6
builtinWords "+" = Just OpPlus
builtinWords "-" = Just OpMinus
builtinWords "mod" = Just OpMod
builtinWords "=" = Just OpEq
builtinWords "!=" = Just OpNe
builtinWords ">" = Just OpGt
builtinWords "<" = Just OpLt
builtinWords ">=" = Just OpGe
builtinWords "<=" = Just OpLe
builtinWords "shl" = Just OpShl
builtinWords "shr" = Just OpShr
builtinWords "band" = Just OpBand
builtinWords "bor" = Just OpBor
builtinWords "print" = Just OpPrint
builtinWords "if" = Just (OpIf JumpNil)
builtinWords "else" = Just (OpElse JumpNil)
builtinWords "while" = Just OpWhile
builtinWords "do" = Just (OpDo JumpNil)
builtinWords "end" = Just (OpEnd JumpNil)
builtinWords "dup" = Just OpDup
builtinWords "2dup" = Just Op2Dup
builtinWords "swap" = Just OpSwap
builtinWords "drop" = Just OpDrop
builtinWords "over" = Just OpOver
builtinWords _ = Nothing

parseWord :: Token -> IO Op
parseWord token =
  let loc = location token
   in case kind token of
        TokenWord w -> case builtinWords w of
          Just op -> return $ Op op loc
          Nothing -> do
            logErrLoc loc ("Unknown word: " % text) w
            throw ParseError
        TokenInt n -> return $ pushInt n loc
        TokenStr s -> return $ pushStr s loc

parseProgram :: [Token] -> IO [Op]
parseProgram = mapM parseWord
