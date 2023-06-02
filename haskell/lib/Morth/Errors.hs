module Morth.Errors where

import Control.Exception (Exception)

data MorthError
  = CommandFailed
  | BadUsage
  | ParseError
  | BlockError
  deriving (Show)

instance Exception MorthError
