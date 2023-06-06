module Morth.Location where

import qualified Data.Text as T
import Data.Text.Lazy.Builder (Builder)
import Formatting (bformat, int, stext, (%))

data Location = Location
  { filePath :: T.Text
  , line :: Int
  , column :: Int
  }
  deriving (Show)

fmtLoc :: Location -> Builder
fmtLoc loc =
  bformat
    (stext % ":" % int % ":" % int)
    (filePath loc)
    (line loc)
    (column loc)
