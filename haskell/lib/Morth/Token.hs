module Morth.Token (Token (..), TokenKind (..), kindReadableName) where

import qualified Data.Text.Lazy as TL
import Morth.Location (Location (..))

data TokenKind
  = TokenWord TL.Text
  | TokenInt Int
  | TokenStr TL.Text
  | TokenChar Char
  deriving (Show)

kindReadableName :: TokenKind -> TL.Text
kindReadableName (TokenWord _) = "word"
kindReadableName (TokenInt _) = "integer"
kindReadableName (TokenStr _) = "string"
kindReadableName (TokenChar _) = "character"

data Token = Token
  { location :: Location
  , kind :: TokenKind
  }
  deriving (Show)
