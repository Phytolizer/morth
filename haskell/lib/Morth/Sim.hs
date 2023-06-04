module Morth.Sim (simulateProgram) where

import Control.Monad.ST (RealWorld)
import Data.Bits ((.&.))
import Data.Primitive (Array, MutableArray, indexArray, newArray, readArray, sizeofArray, writeArray)
import Morth.Config (memCapacity)
import Morth.Op (Op (..), OpCode (..))
import System.IO (Handle, hPrint)

type Stack = [Int]

type Mem = MutableArray RealWorld Int

bop :: (Enum a) => (Int -> Int -> a) -> Stack -> Stack
bop f (y : x : stack) = fromEnum (f x y) : stack
bop _ _ = error "stack underflow"

iter ::
  (Monad m) =>
  (Int -> Op -> Mem -> Stack -> m (Int, Stack)) ->
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
      (ip', stack') <- f ip op mem stack
      iter f ip' mem stack' ops

step :: Handle -> Int -> Op -> Mem -> Stack -> IO (Int, Stack)
step h ip op mem stack = case opCode op of
  OpPush x -> return (ip + 1, x : stack)
  OpDup -> case stack of
    [] -> error "stack underflow"
    x : stack' -> return (ip + 1, x : x : stack')
  OpMem -> return (ip + 1, 0 : stack)
  OpLoad -> case stack of
    [] -> error "stack underflow"
    x : stack' -> do
      y <- readArray mem x
      return (ip + 1, y : stack')
  OpStore -> case stack of
    (value : addr : stack') -> do
      writeArray mem addr (value .&. 0xff)
      return (ip + 1, stack')
    _ -> error "stack underflow"
  OpPlus -> return (ip + 1, bop (+) stack)
  OpMinus -> return (ip + 1, bop (-) stack)
  OpEq -> return (ip + 1, bop (==) stack)
  OpGt -> return (ip + 1, bop (>) stack)
  OpDump -> case stack of
    [] -> error "stack underflow"
    x : stack' -> do
      hPrint h x
      return (ip + 1, stack')
  OpIf (-1) -> error "invalid jump target"
  OpIf dest -> case stack of
    [] -> error "stack underflow"
    x : stack' ->
      return
        ( if x == 0
            then dest
            else ip + 1
        , stack'
        )
  OpElse (-1) -> error "invalid jump target"
  OpElse dest -> return (dest, stack)
  OpWhile -> return (ip + 1, stack)
  OpDo (-1) -> error "invalid jump target"
  OpDo dest -> case stack of
    [] -> error "stack underflow"
    x : stack' ->
      return
        ( if x == 0
            then dest
            else ip + 1
        , stack'
        )
  OpEnd (-1) -> error "invalid jump target"
  OpEnd dest -> return (dest, stack)

loop :: Handle -> Stack -> Array Op -> IO ()
loop h stack ops = do
  arr <- newArray memCapacity 0
  iter (step h) 0 arr stack ops

simulateProgram :: Handle -> Array Op -> IO ()
simulateProgram h = loop h []
