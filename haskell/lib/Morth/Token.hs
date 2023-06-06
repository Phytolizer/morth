module Morth.Token (Token (..), TokenKind (..)) where

import qualified Data.Text.Lazy as TL
import Morth.Location (Location (..))

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
