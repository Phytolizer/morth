module Morth.Parser (parseProgram) where

import Control.Applicative ((<|>))
import Control.Exception (catch, throw)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Primitive (Array)
import Data.Primitive.Array (fromList)
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy as TL
import Formatting (string, text, (%))
import GHC.IO.Exception (IOException (ioe_description))
import Morth.Errors (MorthError (ParseError))
import Morth.Lexer (lexFile)
import Morth.Location (Location)
import Morth.Logger (logErrLoc)
import Morth.Op (
  Jump (JumpNil, JumpTo),
  Op (..),
  OpCode (..),
  mapJmp,
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
builtinWords "*" = Just OpMul
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
builtinWords "macro" = Just OpMacro
builtinWords "end" = Just (OpEnd JumpNil)
builtinWords "include" = Just OpInclude
builtinWords "dup" = Just OpDup
builtinWords "2dup" = Just Op2Dup
builtinWords "swap" = Just OpSwap
builtinWords "drop" = Just OpDrop
builtinWords "over" = Just OpOver
builtinWords _ = Nothing

data Macro = Macro
  { macLoc :: Location
  , macTokens :: [Token]
  }

data State = State
  { stIP :: Int
  , stStack :: [Int]
  , stProgram :: Seq.Seq Op
  , stTokens :: Seq.Seq Token
  , stMacros :: Map.Map TL.Text Macro
  }

parseProgram :: [Token] -> IO (Array Op)
parseProgram ts =
  untilM inputEmpty step (initState ts)
    >>= \st ->
      case stStack st of
        [] -> return (stProgram st & toList & fromList)
        (x : _) -> do
          logErrLoc (location (stTokens st `Seq.index` x)) "Unclosed block"
          throw ParseError
 where
  initState :: [Token] -> State
  initState tokens = State 0 [] Seq.empty (Seq.fromList tokens) Map.empty

  inputEmpty :: State -> Bool
  inputEmpty = Seq.null . stTokens

untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p f = go
 where
  go x | p x = return x
  go x = f x >>= go

step :: State -> IO State
step st =
  let token = Seq.index (stTokens st) 0
      tl = Seq.drop 1 (stTokens st)
   in ( case kind token of
          TokenWord w ->
            fromMaybe
              ( do
                  logErrLoc (location token) ("Unknown word: " % text) w
                  throw ParseError
              )
              $ (builtinWords w <&> return . Right . Op (location token))
                <|> ( Map.lookup w (stMacros st) <&> \m ->
                        return $
                          Left
                            st
                              { stTokens = Seq.fromList (macTokens m) <> tl
                              }
                    )
          TokenInt n -> return $ Right $ pushInt (location token) n
          TokenStr s -> return $ Right $ pushStr (location token) s
          TokenChar c -> return $ Right $ pushInt (location token) (fromEnum c)
      )
        >>= either return (processOp st{stTokens = tl} token)

pop :: [a] -> (a, [a])
pop [] = error "stack underflow"
pop (x : xs) = (x, xs)

unconsSeq :: Seq.Seq a -> Maybe (a, Seq.Seq a)
unconsSeq xs = case Seq.viewl xs of
  Seq.EmptyL -> Nothing
  x Seq.:< xs' -> Just (x, xs')

processOp :: State -> Token -> Op -> IO State
processOp st token op = case opCode op of
  OpIf _ ->
    return
      st
        { stIP = stIP st + 1
        , stStack = stIP st : stStack st
        , stProgram = stProgram st <> Seq.singleton op
        }
  OpElse _ ->
    let program = stProgram st <> Seq.singleton op
        (ifIp, stack) = pop (stStack st)
        ip = stIP st
     in case Seq.index program ifIp & opCode of
          OpIf _ ->
            let program' = program & Seq.adjust (mapJmp $ JumpTo (ip + 1)) ifIp
                stack' = ip : stack
             in return
                  st
                    { stIP = stIP st + 1
                    , stStack = stack'
                    , stProgram = program'
                    }
          _ -> do
            logErrLoc (location token) "else without if"
            throw ParseError
  OpEnd _ ->
    let program = stProgram st <> Seq.singleton op
        (blockIp, stack) = pop (stStack st)
        blockOp = Seq.index program blockIp
        ip = stIP st
     in case opCode blockOp of
          OpIf _ ->
            handleIfElseEnd
              st{stProgram = program, stStack = stack}
              blockIp
              ip
          OpElse _ ->
            handleIfElseEnd
              st{stProgram = program, stStack = stack}
              blockIp
              ip
          OpDo jmp ->
            let program' =
                  program
                    & Seq.adjust (mapJmp jmp) ip
                    & Seq.adjust (mapJmp $ JumpTo (ip + 1)) blockIp
             in return
                  st
                    { stIP = ip + 1
                    , stStack = stack
                    , stProgram = program'
                    }
          _ -> undefined
  OpWhile ->
    return
      st
        { stIP = stIP st + 1
        , stStack = stIP st : stStack st
        , stProgram = stProgram st <> Seq.singleton op
        }
  OpDo _ ->
    let program = stProgram st <> Seq.singleton op
        (whileIp, stack) = pop (stStack st)
        ip = stIP st
        program' = program & Seq.adjust (mapJmp $ JumpTo whileIp) ip
     in return
          st
            { stIP = stIP st + 1
            , stStack = stIP st : stack
            , stProgram = program'
            }
  OpInclude ->
    case unconsSeq (stTokens st) of
      Nothing -> do
        logErrLoc (location token) "include without filename"
        throw ParseError
      Just (nameTok, tl) -> case kind nameTok of
        TokenStr name -> do
          tokens <-
            ( readFile (TL.unpack name)
                >>= lexFile (TL.toStrict name) . TL.pack
                <&> Seq.fromList
                <&> (<> tl)
              )
              `catch` \e -> do
                logErrLoc
                  (location nameTok)
                  ("include of '" % text % "' failed: " % string)
                  name
                  (ioe_description e)
                throw ParseError
          return st{stTokens = tokens}
        _ -> do
          logErrLoc (location nameTok) "expected file name as string literal"
          throw ParseError
  OpMacro ->
    case unconsSeq (stTokens st) of
      Nothing -> do
        logErrLoc (location token) "macro without name"
        throw ParseError
      Just (name, tl) -> case kind name of
        TokenWord w -> case (Map.lookup w (stMacros st) $> ())
          <|> (builtinWords w $> ()) of
          Just _ -> do
            logErrLoc (location name) ("name is already defined: " % text) w
            throw ParseError
          Nothing ->
            readMacro st{stTokens = tl} w (Macro (opLocation op) [])
        _ -> do
          logErrLoc (location name) "expected macro name"
          throw ParseError
  _ ->
    return
      st
        { stIP = stIP st + 1
        , stProgram = stProgram st <> Seq.singleton op
        }

handleIfElseEnd :: (Monad m) => State -> Int -> Int -> m State
handleIfElseEnd st blockIp newDest =
  let program =
        stProgram st
          & Seq.adjust (mapJmp $ JumpTo newDest) blockIp
          & Seq.adjust (mapJmp $ JumpTo (newDest + 1)) newDest
   in return
        st
          { stIP = stIP st + 1
          , stProgram = program
          }

readMacro :: State -> TL.Text -> Macro -> IO State
readMacro st name macro = case unconsSeq (stTokens st) of
  Nothing -> do
    logErrLoc (macLoc macro) "macro without end"
    throw ParseError
  Just (macTok, tl) ->
    let st' = st{stTokens = tl}
     in case kind macTok of
          TokenWord "end" ->
            return
              st'
                { stMacros =
                    Map.insert
                      name
                      macro
                        { macTokens = reverse (macTokens macro)
                        }
                      (stMacros st')
                }
          _ ->
            readMacro
              st'
              name
              macro{macTokens = macTok : macTokens macro}
