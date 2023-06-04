{-# LANGUAGE CPP #-}

module Main (main) where

import Data.Functor ((<&>))
import Morth.Test (recordTest, testFile)
import System.Directory (getDirectoryContents, makeAbsolute)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeExtension, (</>))
import Test.HUnit (Test, Testable (..), runTestTTAndExit)

myPath :: String
myPath = (takeDirectory . takeDirectory) __FILE__

main :: IO ()
main = do
  args <- getArgs
  ts <- case args of
    [] -> tests Nothing <&> Just
    ["test"] -> tests Nothing <&> Just
    ["test", root] -> tests (Just root) <&> Just
    ["record"] -> recordTests Nothing >> return Nothing
    ["record", root] -> recordTests (Just root) >> return Nothing
    _ -> error "invalid arguments"
  mapM_ (runTestTTAndExit . test) ts

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
