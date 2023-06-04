module Morth.Test (testFile, recordTest) where

import Control.Monad (when)
import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Function ((&))
import Morth.Driver (run)
import System.FilePath ((-<.>))
import System.IO.Silently (capture_)
import Test.HUnit (Test, assertFailure, (~:))

assertString :: String -> String -> String -> IO ()
assertString pfx a b =
  when (a /= b) $
    getGroupedDiff (lines a) (lines b)
      & ppDiff
      & ( \diff ->
            assertFailure $ concat [pfx, ": mismatch\n", diff, "\n"]
        )

testFile :: FilePath -> Test
testFile path =
  let txtPath = path -<.> "txt"
   in path
        ~: do
          expectedOutput <- readFile txtPath
          simOutput <- capture_ $ run ["sim", path]
          assertString "sim output is correct" expectedOutput simOutput
          comOutput <- capture_ $ run ["com", "-r", path]
          assertString "com output is correct" expectedOutput comOutput

recordTest :: FilePath -> IO ()
recordTest path =
  let txtPath = path -<.> "txt"
   in capture_ (run ["sim", path])
        >>= writeFile txtPath
