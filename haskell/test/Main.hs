{-# LANGUAGE CPP #-}

module Main (main) where

import Morth.Test (handleArgs)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)

myPath :: String
myPath = (takeDirectory . takeDirectory) __FILE__

main :: IO ()
main = getArgs >>= handleArgs myPath Nothing
