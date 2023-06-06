module Morth.Lexer (lexFile) where

import Data.Char (readLitChar)
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Morth.Token (Location (..), Token (..), TokenKind (..))
import Text.ParserCombinators.ReadP (many, readP_to_S, readS_to_P)
import Text.Read (readMaybe)

lexWord :: TL.Text -> TokenKind
lexWord w = case readMaybe (TL.unpack w) of
  Nothing -> TokenWord w
  Just n -> TokenInt n

lexLine :: T.Text -> Int -> TL.Text -> [Token]
lexLine fp ln = loop 0
 where
  loop :: Int -> TL.Text -> [Token]
  loop col s =
    TL.span (== ' ') s & \(ws, rest) ->
      let wsWidth = fromEnum $ TL.length ws
       in if TL.isPrefixOf "\"" rest
            then
              let (str, rest') = TL.span (/= '"') (TL.tail rest)
                  decodedStr =
                    TL.pack $
                      fst $
                        head $
                          filter ((== "") . snd) $
                            readP_to_S (many (readS_to_P readLitChar)) $
                              TL.unpack str
                  strWidth = fromEnum $ TL.length decodedStr
               in Token (Location fp (ln + 1) (col + 1)) (TokenStr decodedStr)
                    : loop (col + wsWidth + strWidth + 2) (TL.drop 1 rest')
            else
              readWord
                (Location fp (ln + 1) (col + wsWidth + 1))
                (col + wsWidth)
                rest

  readWord :: Location -> Int -> TL.Text -> [Token]
  readWord loc col s
    | TL.null s = []
    | otherwise =
        TL.span (/= ' ') s & \(w, rest) ->
          let wWidth = fromEnum $ TL.length w
           in Token loc (lexWord w)
                : loop (col + wWidth) rest

lexFile :: T.Text -> TL.Text -> [Token]
lexFile fp s = loop 0 (TL.lines s)
 where
  loop :: Int -> [TL.Text] -> [Token]
  loop _ [] = []
  loop ln (l : ls) =
    lexLine fp ln (head $ TL.splitOn "//" l)
      ++ loop (ln + 1) ls
