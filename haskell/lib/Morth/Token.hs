module Morth.Token (Location (..), Token (..), TokenKind (..), fmtLoc) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import Formatting (bformat, int, stext, (%))

data Location = Location
  { filePath :: T.Text
  , line :: Int
  , column :: Int
  }
  deriving (Show)

data TokenKind
  = TokenWord TL.Text
  | TokenInt Int
  | TokenStr TL.Text
  deriving (Show)

data Token = Token
  { location :: Location
  , kind :: TokenKind
  }
  deriving (Show)

fmtLoc :: Location -> Builder
fmtLoc loc =
  bformat
    (stext % ":" % int % ":" % int)
    (filePath loc)
    (line loc)
    (column loc)
