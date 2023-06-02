module MorthLanguage.Token (Location (..), Token (..), fmtLoc) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TLB
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
