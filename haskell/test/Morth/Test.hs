module Morth.Test (handleArgs) where

import Control.Monad (when)
import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Function ((&))
import Data.Functor ((<&>))
import Morth.Driver (run)
import Support.Composition ((.>))
import System.Directory (getDirectoryContents, makeAbsolute)
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.FilePath (takeExtension, (-<.>), (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Silently (capture_)
import Test.HUnit (Test, Testable (test), assertFailure, runTestTTAndExit, (~:))

usage :: IO ()
usage = do
  progName <- getProgName
  mapM_
    (hPutStrLn stderr)
    [ "Usage: " ++ progName ++ " [OPTIONS] [SUBCOMMAND]"
    , "OPTIONS:"
    , "  -f <folder>                 Run tests in a folder"
    , "SUBCOMMANDS:"
    , "  test                         Run tests (default action)"
    , "  record                       Record test expected output"
    ]

handleArgs :: FilePath -> Maybe FilePath -> [String] -> IO ()
handleArgs myPath root args = case args of
  ("-f" : root' : rest) -> handleArgs myPath (Just root') rest
  ["-f"] -> do
    usage
    hPutStrLn stderr "Error: -f requires an argument"
    exitFailure
  ["test"] -> tests root >>= doTest
  [] -> tests root >>= doTest
  ["record"] -> recordTests root
  _ -> do
    usage
    exitFailure

doTest :: [Test] -> IO ()
doTest = runTestTTAndExit . test

tests :: Maybe FilePath -> IO [Test]
tests (Just root) = testPaths root <&> map testFile
tests Nothing = concat <$> mapM (tests . Just) testRoots

recordTests :: Maybe FilePath -> IO ()
recordTests (Just root) = testPaths root >>= mapM_ recordTest
recordTests Nothing = mapM_ (recordTests . Just) testRoots

hasExt :: FilePath -> String -> Bool
hasExt ext p = takeExtension p == ext

testPaths :: FilePath -> IO [FilePath]
testPaths root =
  rel ""
    & getDirectoryContents
    >>= filter (hasExt ".porth")
      .> mapM (rel .> makeAbsolute)
 where
  rel :: FilePath -> FilePath
  rel p = root </> p

testRoots :: [FilePath]
testRoots = ["tests", "examples"]

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
