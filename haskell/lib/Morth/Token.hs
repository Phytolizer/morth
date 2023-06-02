module Morth.Token (Location (..), Token (..), fmtLoc) where

import qualified Data.Text as T
import Formatting (Format, bformat, int, later, stext, (%))

data Location = Location
  { filePath :: T.Text
  , line :: Int
  , column :: Int
  }
  deriving (Show)

data Token = Token
  { location :: Location
  , value :: T.Text
  }
  deriving (Show)

fmtLoc :: Format r (Location -> r)
fmtLoc =
  later $ \loc ->
    bformat
      (stext % ":" % int % ":" % int)
      (filePath loc)
      (line loc)
      (column loc)
