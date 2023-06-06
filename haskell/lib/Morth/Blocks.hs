module Morth.Blocks (resolveBlocks) where

import Control.Exception (throw)
import Control.Monad.ST (RealWorld)
import Data.List (uncons)
import Data.Primitive (Array, MutableArray, readArray, sizeofMutableArray, unsafeFreezeArray, writeArray)
import Morth.Errors (MorthError (BlockError))
import Morth.Logger (logErrLoc)
import Morth.Op (Jump (JumpTo), Op (..), OpCode (..))

resolveBlocks :: MutableArray RealWorld Op -> IO (Array Op)
resolveBlocks = loop 0 []
 where
  loop :: Int -> [Int] -> MutableArray RealWorld Op -> IO (Array Op)
  loop ip stack ops =
    if ip >= sizeofMutableArray ops
      then do
        case stack of
          [] -> unsafeFreezeArray ops
          (top : _) -> do
            topOp <- readArray ops top
            logErrLoc (opLocation topOp) "unclosed block"
            throw BlockError
      else do
        op <- readArray ops ip
        handle ip stack ops op

  handle ip stack ops op =
    case opCode op of
      OpIf _ -> loop (ip + 1) (ip : stack) ops
      OpElse _ -> case uncons stack of
        Nothing -> error "stack underflow"
        Just (ifIp, stack') -> do
          ifOp <- readArray ops ifIp
          case opCode ifOp of
            OpIf _ -> do
              writeArray ops ifIp ifOp{opCode = OpIf (JumpTo $ ip + 1)}
              loop (ip + 1) (ip : stack') ops
            _ -> do
              logErrLoc (opLocation op) "expected if"
              throw BlockError
      OpWhile -> loop (ip + 1) (ip : stack) ops
      OpDo _ -> case uncons stack of
        Nothing -> error "stack underflow"
        Just (whileIp, stack') -> do
          writeArray ops ip op{opCode = OpDo (JumpTo whileIp)}
          loop (ip + 1) (ip : stack') ops
      OpEnd _ -> case uncons stack of
        Nothing -> error "stack underflow"
        Just (blockIp, stack') -> do
          blockOp <- readArray ops blockIp
          case opCode blockOp of
            OpIf _ -> do
              writeArray ops blockIp blockOp{opCode = OpIf (JumpTo ip)}
              writeArray ops ip op{opCode = OpEnd (JumpTo $ ip + 1)}
              loop (ip + 1) stack' ops
            OpElse _ -> do
              writeArray ops blockIp blockOp{opCode = OpElse (JumpTo ip)}
              writeArray ops ip op{opCode = OpEnd (JumpTo $ ip + 1)}
              loop (ip + 1) stack' ops
            OpDo dest -> do
              writeArray ops ip op{opCode = OpEnd dest}
              writeArray ops blockIp blockOp{opCode = OpDo (JumpTo $ ip + 1)}
              loop (ip + 1) stack' ops
            _ -> do
              logErrLoc (opLocation op) "mismatched 'end': expected if, else, or do"
              throw BlockError
      _ -> loop (ip + 1) stack ops
