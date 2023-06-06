module Morth.Lexer (lexFile) where

import Control.Monad (liftM2, replicateM)
import Data.Bifunctor (second)
import Data.Char (isDigit)
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Morth.Location (Location (..))
import Morth.Token (Token (..), TokenKind (..))
import Support.Composition ((.>))
import Text.Parsec (parserReturn, (<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Text.Lazy as PL
import Text.Read (readMaybe)

escChar :: Char -> Int -> PL.Parser Int
escChar c res = P.char '\\' >> P.char c >> parserReturn res

escChars :: [PL.Parser Int]
escChars =
  map
    (P.try . uncurry escChar . second (fromIntegral . fromEnum))
    [ ('\\', '\\')
    , ('\'', '\'')
    , ('"', '"')
    , ('b', '\b')
    , ('f', '\f')
    , ('n', '\n')
    , ('t', '\t')
    , ('r', '\r')
    , ('v', '\v')
    , ('a', '\a')
    ]

uptoN :: Int -> PL.Parser a -> PL.Parser [a]
uptoN 0 _ = parserReturn []
uptoN n p =
  foldr
    ((<|>) . (\x -> P.try (replicateM x p)))
    (parserReturn [])
    [1 .. n]

uptoN1 :: Int -> PL.Parser a -> PL.Parser [a]
uptoN1 n p =
  liftM2 (++) (replicateM 1 p) (uptoN (n - 1) p)

readInt :: Int -> (Char -> Int) -> String -> Int
readInt base f =
  foldl1 ((+) . (* base)) . map f

valDigit :: Int -> Char -> Int
valDigit base c
  | base <= 10 = fromEnum c - fromEnum '0'
  | otherwise = case c of
      c' | isDigit c' -> fromEnum c' - fromEnum '0'
      c' | c' >= 'a' && c' <= 'f' -> fromEnum c' - fromEnum 'a' + 10
      c' | c' >= 'A' && c' <= 'F' -> fromEnum c' - fromEnum 'A' + 10
      _ -> error "unreachable"

octalEsc :: PL.Parser Int
octalEsc =
  (P.char '\\' >> uptoN1 3 P.octDigit)
    >>= readInt 8 (valDigit 8) .> parserReturn

parseHex :: Int -> PL.Parser Int
parseHex n =
  P.count n P.hexDigit
    >>= readInt 16 (valDigit 16) .> parserReturn

hexEsc :: PL.Parser Int
hexEsc = P.string "\\x" >> parseHex 2

lowerUEsc :: PL.Parser Int
lowerUEsc = P.string "\\u" >> parseHex 4

upperUEsc :: PL.Parser Int
upperUEsc = P.string "\\U" >> parseHex 8

esc :: PL.Parser [Int]
esc =
  (++)
    <$> P.many
      ( P.choice $
          escChars
            ++ [ octalEsc
               , hexEsc
               , lowerUEsc
               , upperUEsc
               , P.anyChar >>= fromEnum .> parserReturn
               ]
      )
    <*> (P.eof >> parserReturn [])

readEscapes :: TL.Text -> TL.Text
readEscapes =
  P.parse esc ""
    .> either (error . show) id
    .> map (toEnum . fromIntegral)
    .> TL.pack

lexWord :: TL.Text -> TokenKind
lexWord w = case readMaybe (TL.unpack w) of
  Nothing -> TokenWord w
  Just n -> TokenInt n

mapFst :: (a -> a') -> (a, b) -> (a', b)
mapFst f (a, b) = (f a, b)

lexLine :: T.Text -> Int -> TL.Text -> [Token]
lexLine fp ln = loop 0
 where
  loop :: Int -> TL.Text -> [Token]
  loop col s =
    TL.span (== ' ') s & \(ws, rest) ->
      let wsWidth = fromEnum $ TL.length ws
       in if TL.isPrefixOf "\"" rest
            then
              let (str, rest') =
                    TL.span (/= '"') (TL.tail rest)
                      & mapFst readEscapes
                  strWidth = fromEnum $ TL.length str
               in Token (Location fp (ln + 1) (col + 1)) (TokenStr str)
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
