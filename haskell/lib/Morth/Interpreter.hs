module Morth.Interpreter (interpretProgram, ByteOrder (..)) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Primitive (Array, indexArray, sizeofArray)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.IO as TLIO
import Morth.Config (memCapacity, strCapacity)
import Morth.OS (OS (Linux))
import Morth.Op (Jump (..), Op (..), OpCode (..), Value (..))
import System.IO (Handle, hPrint, stderr)
import System.Process (getCurrentPid)
import Text.Printf (hPrintf)

data ByteOrder = LittleEndian | BigEndian

type Stack = [Int]

type IP = Int

type Mem = BL.ByteString

bop :: (Enum a) => (Int -> Int -> a) -> State -> State
bop f state = case stack state of
  (y : x : stack') -> state{stack = fromEnum (f x y) : stack'}
  _ -> error "stack underflow"

data State = State
  { mem :: Mem
  , strSize :: Int
  , strOffsets :: Map.Map Int Int
  , stack :: Stack
  }

type StepFunc = IP -> Op -> State -> IO (Int, State)

iter :: StepFunc -> IP -> State -> Array Op -> IO ()
iter f ip state ops
  | sizeofArray ops == 0 = return ()
  | ip >= sizeofArray ops = return ()
  | otherwise = do
      let op = indexArray ops ip
      (ip', state') <- f ip op state
      iter f ip' state' ops

step :: Handle -> StepFunc
step h ip op state = case opCode op of
  OpPush (ValInt x) -> return (ip + 1, state{stack = x : stack state})
  OpPush (ValStr s) ->
    let bs = encodeUtf8 s
        n = fromIntegral $ BL.length bs
        (offset, mem', strOffsets') = case Map.lookup n (strOffsets state) of
          Just o -> (o, mem state, strOffsets state)
          Nothing ->
            let lhs = BL.take (fromIntegral $ strSize state) (mem state)
                rhs = BL.drop (fromIntegral $ strSize state + n) (mem state)
             in ( strSize state
                , lhs <> bs <> rhs
                , Map.insert n (strSize state) (strOffsets state)
                )
     in return
          ( ip + 1
          , state
              { stack = offset : n : stack state
              , mem = mem'
              , strSize = strSize state + n
              , strOffsets = strOffsets'
              }
          )
  OpDup -> case stack state of
    [] -> error "stack underflow"
    x : stack' -> return (ip + 1, state{stack = x : x : stack'})
  Op2Dup -> case stack state of
    (y : x : stack') -> return (ip + 1, state{stack = y : x : y : x : stack'})
    _ -> error "stack underflow"
  OpSwap -> case stack state of
    (y : x : stack') -> return (ip + 1, state{stack = x : y : stack'})
    _ -> error "stack underflow"
  OpDrop -> case stack state of
    (_ : stack') -> return (ip + 1, state{stack = stack'})
    _ -> error "stack underflow"
  OpOver -> case stack state of
    (y : x : stack') -> return (ip + 1, state{stack = x : y : x : stack'})
    _ -> error "stack underflow"
  OpMem -> return (ip + 1, state{stack = strCapacity : stack state})
  OpLoad -> case stack state of
    [] -> error "stack underflow"
    x : stack' ->
      let y = fromIntegral $ BL.index (mem state) (toEnum x)
       in return (ip + 1, state{stack = y : stack'})
  OpStore -> case stack state of
    (value : addr : stack') ->
      let addr' = toEnum addr
          lhs = BL.take addr' (mem state)
          rhs = BL.drop (addr' + 1) (mem state)
       in return
            ( ip + 1
            , state
                { stack = stack'
                , mem = lhs <> BL.cons (fromIntegral value) rhs
                }
            )
    _ -> error "stack underflow"
  OpSyscall0 -> case stack state of
    (syscall : stack') -> do
      case syscall of
        39 -> do
          pid <- fromIntegral <$> getCurrentPid
          return (ip + 1, state{stack = pid : stack'})
        _ -> do
          hPrintf stderr "unimplemented syscall: %d\n" syscall
          error "unimplemented"
    _ -> error "stack underflow"
  OpSyscall1 -> error "unimplemented"
  OpSyscall2 -> error "unimplemented"
  OpSyscall3 -> case stack state of
    (syscall : arg1 : arg2 : arg3 : stack') -> do
      case syscall of
        1 ->
          let fd = arg1
              buf = arg2
              count = arg3
              s =
                mem state
                  & BL.drop (toEnum buf)
                  & BL.take (toEnum count)
                  & decodeUtf8
           in do
                case fd of
                  1 -> TLIO.hPutStr h s
                  2 -> TLIO.hPutStr stderr s
                  _ -> do
                    hPrintf stderr "unimplemented fd: %d\n" fd
                    error "unimplemented"
                return (ip + 1, state{stack = count : stack'})
        _ -> do
          hPrintf stderr "unimplemented syscall: %d\n" syscall
          error "unimplemented"
    _ -> error "stack underflow"
  OpSyscall4 -> error "unimplemented"
  OpSyscall5 -> error "unimplemented"
  OpSyscall6 -> error "unimplemented"
  OpPlus -> return (ip + 1, bop (+) state)
  OpMinus -> return (ip + 1, bop (-) state)
  OpMul -> return (ip + 1, bop (*) state)
  OpMod -> return (ip + 1, bop mod state)
  OpEq -> return (ip + 1, bop (==) state)
  OpNe -> return (ip + 1, bop (/=) state)
  OpGt -> return (ip + 1, bop (>) state)
  OpLt -> return (ip + 1, bop (<) state)
  OpGe -> return (ip + 1, bop (>=) state)
  OpLe -> return (ip + 1, bop (<=) state)
  OpShr -> return (ip + 1, bop shiftR state)
  OpShl -> return (ip + 1, bop shiftL state)
  OpBor -> return (ip + 1, bop (.|.) state)
  OpBand -> return (ip + 1, bop (.&.) state)
  OpPrint -> case stack state of
    [] -> error "stack underflow"
    x : stack' -> do
      hPrint h x
      return (ip + 1, state{stack = stack'})
  OpIf JumpNil -> error "invalid jump target"
  OpIf (JumpTo dest) -> case stack state of
    [] -> error "stack underflow"
    x : stack' ->
      return
        ( if x == 0
            then dest
            else ip + 1
        , state{stack = stack'}
        )
  OpElse JumpNil -> error "invalid jump target"
  OpElse (JumpTo dest) -> return (dest, state)
  OpWhile -> return (ip + 1, state)
  OpDo JumpNil -> error "invalid jump target"
  OpDo (JumpTo dest) -> case stack state of
    [] -> error "stack underflow"
    x : stack' ->
      return
        ( if x == 0
            then dest
            else ip + 1
        , state{stack = stack'}
        )
  OpMacro -> undefined
  OpInclude -> undefined
  OpEnd JumpNil -> error "invalid jump target"
  OpEnd (JumpTo dest) -> return (dest, state)

interpretProgram :: ByteOrder -> OS -> Handle -> Array Op -> IO ()
interpretProgram LittleEndian Linux h =
  iter (step h) 0 $
    State
      { mem = BL.replicate (fromIntegral (memCapacity + strCapacity)) 0
      , strSize = 0
      , strOffsets = Map.empty
      , stack = []
      }
interpretProgram _ _ _ = error "unimplemented"
