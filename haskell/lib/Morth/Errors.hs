module Morth.Errors where

import Control.Exception (Exception)

data CommandFailError = CommandFailed deriving (Show)

instance Exception CommandFailError

data BadUsage = BadUsage deriving (Show)

instance Exception BadUsage

data ParseError = ParseError deriving (Show)

instance Exception ParseError
