module Morth.Blocks (resolveBlocks) where

import Control.Monad.ST (RealWorld)
import Data.List (uncons)
import Data.Primitive (Array, MutableArray, readArray, sizeofMutableArray, unsafeFreezeArray, writeArray)
import Morth.Op (Op (..))

resolveBlocks :: MutableArray RealWorld Op -> IO (Array Op)
resolveBlocks = loop 0 []
 where
  loop :: Int -> [Int] -> MutableArray RealWorld Op -> IO (Array Op)
  loop ip stack ops =
    if ip >= sizeofMutableArray ops
      then unsafeFreezeArray ops
      else do
        op <- readArray ops ip
        case op of
          OpIf _ -> loop (ip + 1) (ip : stack) ops
          OpEnd -> case uncons stack of
            Nothing -> error "stack underflow"
            Just (ifIp, stack') -> do
              ifOp <- readArray ops ifIp
              case ifOp of
                OpIf _ -> do
                  writeArray ops ifIp (OpIf ip)
                  loop (ip + 1) stack' ops
                _ -> error "expected if"
          _ -> loop (ip + 1) stack ops
