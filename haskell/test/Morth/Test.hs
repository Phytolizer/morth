module Morth.Test (testFile, recordTest) where

import Morth.Driver (run)
import System.FilePath ((-<.>))
import System.IO.Silently (capture_)
import Test.HUnit (
  Test,
  (@=?),
  (~:),
 )

testFile :: FilePath -> Test
testFile path =
  let txtPath = path -<.> "txt"
   in path
        ~: do
          expectedOutput <- readFile txtPath
          simOutput <- capture_ $ run ["sim", path]
          simOutput @=? expectedOutput
          comOutput <- capture_ $ run ["com", "-r", path]
          comOutput @=? expectedOutput

recordTest :: FilePath -> IO ()
recordTest path =
  let txtPath = path -<.> "txt"
   in capture_ (run ["sim", path])
        >>= writeFile txtPath
