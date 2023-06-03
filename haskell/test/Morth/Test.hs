module Morth.Test (testFile) where

import Morth.Driver (run)
import System.IO.Silently (capture_)
import Test.HUnit (
  Test,
  (@=?),
  (~:),
 )

testFile :: FilePath -> Test
testFile path =
  let path' = path
   in path' ~:
        do
          simOutput <- capture_ $ run ["sim", path']
          comOutput <- capture_ $ run ["com", "-r", path']
          simOutput @=? comOutput
