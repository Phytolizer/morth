module Morth.Sim (simulateProgram) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import Data.Primitive (Array, indexArray, sizeofArray)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO as TLIO
import Morth.Config (memCapacity)
import Morth.Op (Op (..), OpCode (..))
import System.IO (Handle, hPrint, stderr)
import System.Process (getCurrentPid)
import Text.Printf (hPrintf)

type Stack = [Int]

type Mem = BL.ByteString

bop :: (Enum a) => (Int -> Int -> a) -> Stack -> Stack
bop f (y : x : stack) = fromEnum (f x y) : stack
bop _ _ = error "stack underflow"

iter ::
  (Monad m) =>
  (Int -> Op -> Mem -> Stack -> m (Int, Stack, Mem)) ->
  Int ->
  Mem ->
  Stack ->
  Array Op ->
  m ()
iter f ip mem stack ops
  | sizeofArray ops == 0 = return ()
  | ip >= sizeofArray ops = return ()
  | otherwise = do
      let op = indexArray ops ip
      (ip', stack', mem') <- f ip op mem stack
      iter f ip' mem' stack' ops

step :: Handle -> Int -> Op -> Mem -> Stack -> IO (Int, Stack, Mem)
step h ip op mem stack = case opCode op of
  OpPush x -> return (ip + 1, x : stack, mem)
  OpDup -> case stack of
    [] -> error "stack underflow"
    x : stack' -> return (ip + 1, x : x : stack', mem)
  Op2Dup -> case stack of
    (y : x : stack') -> return (ip + 1, y : x : y : x : stack', mem)
    _ -> error "stack underflow"
  OpSwap -> case stack of
    (y : x : stack') -> return (ip + 1, x : y : stack', mem)
    _ -> error "stack underflow"
  OpDrop -> case stack of
    (_ : stack') -> return (ip + 1, stack', mem)
    _ -> error "stack underflow"
  OpOver -> case stack of
    (y : x : stack') -> return (ip + 1, x : y : x : stack', mem)
    _ -> error "stack underflow"
  OpMem -> return (ip + 1, 0 : stack, mem)
  OpLoad -> case stack of
    [] -> error "stack underflow"
    x : stack' ->
      let y = fromIntegral $ BL.index mem (toEnum x)
       in return (ip + 1, y : stack', mem)
  OpStore -> case stack of
    (value : addr : stack') ->
      let addr' = toEnum addr
          lhs = BL.take addr' mem
          rhs = BL.drop (addr' + 1) mem
       in return
            ( ip + 1
            , stack'
            , BL.append lhs $ BL.cons (fromIntegral value) rhs
            )
    _ -> error "stack underflow"
  OpSyscall0 -> case stack of
    (syscall : stack') -> do
      case syscall of
        39 -> do
          pid <- fromIntegral <$> getCurrentPid
          return (ip + 1, pid : stack', mem)
        _ -> do
          hPrintf stderr "unimplemented syscall: %d\n" syscall
          error "unimplemented"
    _ -> error "stack underflow"
  OpSyscall1 -> error "unimplemented"
  OpSyscall2 -> error "unimplemented"
  OpSyscall3 -> case stack of
    (syscall : arg1 : arg2 : arg3 : stack') -> do
      case syscall of
        1 ->
          let fd = arg1
              buf = arg2
              count = arg3
              s =
                mem
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
                return (ip + 1, count : stack', mem)
        _ -> do
          hPrintf stderr "unimplemented syscall: %d\n" syscall
          error "unimplemented"
    _ -> error "stack underflow"
  OpSyscall4 -> error "unimplemented"
  OpSyscall5 -> error "unimplemented"
  OpSyscall6 -> error "unimplemented"
  OpPlus -> return (ip + 1, bop (+) stack, mem)
  OpMinus -> return (ip + 1, bop (-) stack, mem)
  OpMod -> return (ip + 1, bop mod stack, mem)
  OpEq -> return (ip + 1, bop (==) stack, mem)
  OpNe -> return (ip + 1, bop (/=) stack, mem)
  OpGt -> return (ip + 1, bop (>) stack, mem)
  OpLt -> return (ip + 1, bop (<) stack, mem)
  OpGe -> return (ip + 1, bop (>=) stack, mem)
  OpLe -> return (ip + 1, bop (<=) stack, mem)
  OpShr -> return (ip + 1, bop shiftR stack, mem)
  OpShl -> return (ip + 1, bop shiftL stack, mem)
  OpBor -> return (ip + 1, bop (.|.) stack, mem)
  OpBand -> return (ip + 1, bop (.&.) stack, mem)
  OpDump -> case stack of
    [] -> error "stack underflow"
    x : stack' -> do
      hPrint h x
      return (ip + 1, stack', mem)
  OpIf (-1) -> error "invalid jump target"
  OpIf dest -> case stack of
    [] -> error "stack underflow"
    x : stack' ->
      return
        ( if x == 0
            then dest
            else ip + 1
        , stack'
        , mem
        )
  OpElse (-1) -> error "invalid jump target"
  OpElse dest -> return (dest, stack, mem)
  OpWhile -> return (ip + 1, stack, mem)
  OpDo (-1) -> error "invalid jump target"
  OpDo dest -> case stack of
    [] -> error "stack underflow"
    x : stack' ->
      return
        ( if x == 0
            then dest
            else ip + 1
        , stack'
        , mem
        )
  OpEnd (-1) -> error "invalid jump target"
  OpEnd dest -> return (dest, stack, mem)

loop :: Handle -> Stack -> Array Op -> IO ()
loop h stack ops = do
  iter (step h) 0 (BL.replicate (fromIntegral memCapacity) 0) stack ops

simulateProgram :: Handle -> Array Op -> IO ()
simulateProgram h = loop h []
