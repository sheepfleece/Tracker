module Util.BEncode (parse, compose, BValue(..), byteString, list, int, dictionary) where

import           ClassyPrelude.Yesod              hiding (take)

import           Data.Attoparsec.ByteString       hiding (parse)
import           Data.Attoparsec.ByteString.Char8 (char, decimal, signed)

import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS (length, readFile)
import qualified Data.ByteString.Char8            as BS (pack)

type Dictionary = [(ByteString, BValue)]

data BValue = BString ByteString
    | BInteger Int
    | BList [BValue]
    | BDictionary Dictionary
    deriving (Show, Eq)

instance IsString BValue where
  fromString = BString . fromString

parse :: ByteString -> Either String BValue
parse = parseOnly parseBFile

showBS :: (Show a) => a -> ByteString
showBS = BS.pack . show

compose :: BValue -> ByteString
compose (BString     s ) = (showBS . BS.length) s <> ":" <> s
compose (BInteger    i ) = "i" <> showBS i <> "e"
compose (BList       ls) = "l" <> foldMap compose ls <> "e"
compose (BDictionary d ) = "d" <> foldMap (uncurry each) d <> "e"
 where
  each :: ByteString -> BValue -> ByteString
  each k v = showBS (BS.length k) <> ":" <> k <> compose v

parseBFile :: Parser BValue
parseBFile = parseBValue <* endOfInput

parseBValue :: Parser BValue
parseBValue = choice [parseInt, parseString, parseList, parseDictionary]

parseInt :: Parser BValue
parseInt = BInteger <$> (char 'i' *> signed decimal <* char 'e')

parseString :: Parser BValue
parseString = do
  len <- decimal
  _   <- char ':'
  BString <$> take len

parseList :: Parser BValue
parseList = BList <$> (char 'l' *> many' parseBValue <* char 'e')

parseDictionaryElement :: Parser (ByteString, BValue)
parseDictionaryElement = do
  (BString str) <- parseString
  val           <- parseBValue
  pure (str, val)

parseDictionary :: Parser BValue
parseDictionary = do
  _  <- char 'd'
  ls <- many' parseDictionaryElement
  _  <- char 'e'
  pure $ BDictionary $ ls


byteString :: BValue -> Maybe ByteString
byteString (BString v) = Just v
byteString _           = Nothing

list :: BValue -> Maybe [BValue]
list (BList ls) = Just ls
list _          = Nothing

int :: BValue -> Maybe Int
int (BInteger i) = Just i
int _            = Nothing

dictionary :: BValue -> Maybe [(ByteString, BValue)]
dictionary = \case
  (BDictionary d) -> Just d
  _               -> Nothing

