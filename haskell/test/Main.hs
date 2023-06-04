{-# LANGUAGE CPP #-}

module Main (main) where

import Data.Functor ((<&>))
import Morth.Test (recordTest, testFile)
import System.Directory (getDirectoryContents, makeAbsolute)
import System.Directory.Internal.Prelude (exitFailure)
import System.Environment (getArgs, getProgName)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import Test.HUnit (Test, Testable (..), runTestTTAndExit)

myPath :: String
myPath = (takeDirectory . takeDirectory) __FILE__

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

handleArgs :: Maybe FilePath -> [String] -> IO ()
handleArgs root args = case args of
  ("-f" : root' : rest) -> handleArgs (Just root') rest
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

main :: IO ()
main = getArgs >>= handleArgs Nothing

doTest :: [Test] -> IO ()
doTest = runTestTTAndExit . test

tests :: Maybe FilePath -> IO [Test]
tests (Just root) = testPaths root <&> map testFile
tests Nothing = concat <$> mapM (tests . Just) testRoots

recordTests :: Maybe FilePath -> IO ()
recordTests (Just root) = testPaths root >>= mapM_ recordTest
recordTests Nothing = mapM_ (recordTests . Just) testRoots

testPaths :: FilePath -> IO [FilePath]
testPaths root =
  mapM (\p -> makeAbsolute (myPath </> root </> p))
    . filter (\p -> takeExtension p == ".porth")
    =<< getDirectoryContents (myPath </> root)

testRoots :: [FilePath]
testRoots = ["tests", "examples"]
