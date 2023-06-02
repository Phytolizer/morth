module Morth.Sim (simulateProgram) where

import Data.Primitive (Array, indexArray, sizeofArray)
import Morth.Op (Op (..))
import System.IO (Handle, hPrint)

type Stack = [Int]

bop :: (Enum a) => (Int -> Int -> a) -> Stack -> Stack
bop f (y : x : stack) = fromEnum (f x y) : stack
bop _ _ = error "stack underflow"

iter ::
  (Monad m) =>
  (Int -> Op -> Stack -> m (Int, Stack)) ->
  Int ->
  Stack ->
  Array Op ->
  m ()
iter f ip stack ops
  | sizeofArray ops == 0 = return ()
  | ip >= sizeofArray ops = return ()
  | otherwise = do
      let op = indexArray ops ip
      (ip', stack') <- f ip op stack
      iter f ip' stack' ops

step :: Handle -> Int -> Op -> Stack -> IO (Int, Stack)
step h ip op stack = case op of
  OpPush x -> return (ip + 1, x : stack)
  OpDup -> case stack of
    [] -> error "stack underflow"
    x : stack' -> return (ip + 1, x : x : stack')
  OpPlus -> return (ip + 1, bop (+) stack)
  OpMinus -> return (ip + 1, bop (-) stack)
  OpEq -> return (ip + 1, bop (==) stack)
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
  OpEnd -> return (ip + 1, stack)

loop :: Handle -> Stack -> Array Op -> IO ()
loop h = iter (step h) 0

simulateProgram :: Handle -> Array Op -> IO ()
simulateProgram h = loop h []
