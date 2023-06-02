module MorthLanguage.Sim (simulateProgram) where

import MorthLanguage.Op (Op (..))
import System.IO (Handle, hPrint)

bop :: (Enum a) => (Int -> Int -> a) -> [Int] -> [Int]
bop f (y : x : stack) = fromEnum (f x y) : stack
bop _ _ = error "stack underflow"

iter :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m ()
iter _ _ [] = return ()
iter f z (x : xs) = do
  z' <- f x z
  iter f z' xs

step :: Handle -> Op -> [Int] -> IO [Int]
step h op stack = case op of
  OpPush x -> return (x : stack)
  OpPlus -> return $ bop (+) stack
  OpMinus -> return $ bop (-) stack
  OpEq -> return $ bop (==) stack
  OpDump -> case stack of
    [] -> error "stack underflow"
    x : stack' -> do
      hPrint h x
      return stack'

loop :: Handle -> [Int] -> [Op] -> IO ()
loop h = iter (step h)

simulateProgram :: Handle -> [Op] -> IO ()
simulateProgram h = loop h []
