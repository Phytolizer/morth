{-# LANGUAGE CPP #-}

module Main (main) where

import Morth.Test (recordTest, testFile)
import System.Directory (getDirectoryContents, makeAbsolute)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeExtension, (</>))
import Test.HUnit (Testable (..), runTestTTAndExit)

myPath :: String
myPath = (takeDirectory . takeDirectory) __FILE__

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runTests
    ["test"] -> runTests
    ["record"] -> recordTests
    _ -> error "invalid arguments"

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

runTests :: IO ()
runTests = testPaths >>= map testFile .> (test .> runTestTTAndExit)

recordTests :: IO ()
recordTests = testPaths >>= mapM_ recordTest

testPaths :: IO [FilePath]
testPaths =
  mapM (\p -> makeAbsolute (myPath </> "tests" </> p))
    . filter (\p -> takeExtension p == ".porth")
    =<< getDirectoryContents (myPath </> "tests")
