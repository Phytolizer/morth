module Morth.Lexer (lexFile) where

import Control.Applicative (liftA2)
import Control.Exception (throw)
import Control.Monad (liftM2, replicateM)
import Data.Bifunctor (second)
import Data.Char (isDigit)
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Formatting (text, (%))
import Morth.Errors (MorthError (LexError))
import Morth.Location (Location (..))
import Morth.Logger (logErrLoc)
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

data Quote = SingleQuote | DoubleQuote

isQuote :: TL.Text -> Maybe Quote
isQuote s
  | TL.isPrefixOf "'" s = Just SingleQuote
  | TL.isPrefixOf "\"" s = Just DoubleQuote
  | otherwise = Nothing

quoteChar :: Quote -> Char
quoteChar SingleQuote = '\''
quoteChar DoubleQuote = '"'

quoteStr :: Quote -> TL.Text
quoteStr SingleQuote = "'"
quoteStr DoubleQuote = "\""

litName :: Quote -> TL.Text
litName SingleQuote = "character"
litName DoubleQuote = "string"

lexLine :: T.Text -> Int -> TL.Text -> IO [Token]
lexLine fp ln = loop 0
 where
  loop :: Int -> TL.Text -> IO [Token]
  loop col s =
    TL.span (== ' ') s & \(ws, rest) ->
      let wsWidth = fromEnum $ TL.length ws
       in case isQuote rest of
            Just q ->
              let (str, rest') =
                    TL.span (/= quoteChar q) (TL.tail rest)
                      & mapFst readEscapes
                  strWidth = fromEnum $ TL.length str
                  loc = Location fp (ln + 1) (col + 1)
               in if TL.isPrefixOf (quoteStr q) rest'
                    then do
                      tokenKind <- case q of
                        DoubleQuote -> return $ TokenStr str
                        SingleQuote -> case TL.uncons str of
                          Just (c, tl) | TL.null tl -> return $ TokenChar c
                          _ -> do
                            logErrLoc loc ("invalid " % text % " literal") (litName q)
                            throw LexError
                      fmap
                        (Token loc tokenKind :)
                        (loop (col + wsWidth + strWidth + 2) (TL.drop 1 rest'))
                    else do
                      logErrLoc loc ("unterminated " % text % "literal") (litName q)
                      throw LexError
            Nothing ->
              readWord
                (Location fp (ln + 1) (col + wsWidth + 1))
                (col + wsWidth)
                rest

  readWord :: Location -> Int -> TL.Text -> IO [Token]
  readWord loc col s
    | TL.null s = return []
    | otherwise =
        TL.span (/= ' ') s & \(w, rest) ->
          let wWidth = fromEnum $ TL.length w
           in fmap
                (Token loc (lexWord w) :)
                (loop (col + wWidth) rest)

lexFile :: T.Text -> TL.Text -> IO [Token]
lexFile fp s = loop 0 (TL.lines s)
 where
  loop :: Int -> [TL.Text] -> IO [Token]
  loop _ [] = return []
  loop ln (l : ls) =
    liftA2
      (++)
      (lexLine fp ln (head $ TL.splitOn "//" l))
      (loop (ln + 1) ls)
