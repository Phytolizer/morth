module MorthLanguage.Lexer (lexFile) where

import Data.Function ((&))
import Data.List (findIndex)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import MorthLanguage.Token (Location (..), Token (..))

lexLine :: T.Text -> Int -> TL.Text -> [Token]
lexLine fp ln s = loop 0 s
 where
  loop :: Int -> TL.Text -> [Token]
  loop col s =
    TL.span (== ' ') s & \(ws, rest) ->
      let wsWidth = fromEnum $ TL.length ws
       in readWord
            (Location fp (ln + 1) (col + wsWidth + 1))
            (col + wsWidth)
            rest

  readWord :: Location -> Int -> TL.Text -> [Token]
  readWord loc col s
    | TL.null s = []
    | otherwise =
        TL.span (/= ' ') s & \(w, rest) ->
          let wWidth = fromEnum $ TL.length w
           in Token loc (TL.toStrict w)
                : loop (col + wWidth) rest

lexFile :: T.Text -> TL.Text -> [Token]
lexFile fp s = loop 0 (TL.lines s)
 where
  loop :: Int -> [TL.Text] -> [Token]
  loop _ [] = []
  loop ln (l : ls) = lexLine fp ln l ++ loop (ln + 1) ls
