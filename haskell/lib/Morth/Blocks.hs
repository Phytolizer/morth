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
        handle ip stack ops op

  handle ip stack ops op =
    case op of
      OpIf _ -> loop (ip + 1) (ip : stack) ops
      OpElse _ -> case uncons stack of
        Nothing -> error "stack underflow"
        Just (ifIp, stack') -> do
          ifOp <- readArray ops ifIp
          case ifOp of
            OpIf _ -> do
              writeArray ops ifIp (OpIf (ip + 1))
              loop (ip + 1) (ip : stack') ops
            _ -> error "expected if"
      OpWhile -> loop (ip + 1) (ip : stack) ops
      OpDo _ -> case uncons stack of
        Nothing -> error "stack underflow"
        Just (whileIp, stack') -> do
          writeArray ops ip (OpDo whileIp)
          loop (ip + 1) (ip : stack') ops
      OpEnd _ -> case uncons stack of
        Nothing -> error "stack underflow"
        Just (blockIp, stack') -> do
          blockOp <- readArray ops blockIp
          case blockOp of
            OpIf _ -> do
              writeArray ops blockIp (OpIf ip)
              writeArray ops ip (OpEnd (ip + 1))
              loop (ip + 1) stack' ops
            OpElse _ -> do
              writeArray ops blockIp (OpElse ip)
              writeArray ops ip (OpEnd (ip + 1))
              loop (ip + 1) stack' ops
            OpDo dest -> do
              writeArray ops ip (OpEnd dest)
              writeArray ops blockIp (OpDo (ip + 1))
              loop (ip + 1) stack' ops
            _ -> error "expected if"
      _ -> loop (ip + 1) stack ops
