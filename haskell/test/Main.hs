{-# LANGUAGE CPP #-}

module Main where

import Morth.Test (testFile)
import System.Directory (getDirectoryContents, makeAbsolute)
import System.FilePath (takeDirectory, takeExtension, (</>))
import Test.HUnit (Testable (..), runTestTTAndExit)

myPath :: String
myPath = (takeDirectory . takeDirectory) __FILE__

main :: IO ()
main =
  (runTestTTAndExit . test)
    =<< mapM (\p -> testFile <$> makeAbsolute (myPath </> "tests" </> p))
      . filter (\p -> takeExtension p == ".porth")
    =<< getDirectoryContents (myPath </> "tests")
