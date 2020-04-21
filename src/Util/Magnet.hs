module Util.Magnet (tr, dn, xt, magnet, Magnet) where

import qualified ByteString.StrictBuilder as BS
import           ClassyPrelude.Yesod
import           Data.Char
import           Data.Map.Merge.Strict
import           Util.Hash

newtype Magnet = MkMagnet { unMagnet :: BS.Builder }

instance Semigroup Magnet where
  (MkMagnet m1) <> (MkMagnet m2) = MkMagnet $ m1 <> "&" <> m2

(=:) :: BS.Builder -> Text -> Magnet
name =: option = MkMagnet $ name <> "=" <> option'
  where
    option' = escape $ encodeUtf8 option

escape :: ByteString -> BS.Builder
escape = foldMap each
 where
  each :: Word8 -> BS.Builder
  each c
    | c > 96 && c < 123
      || c > 64 && c < 91
      || c > 47 && c < 58
      || c == ord' '.'
      || c == ord' '~'
      || c == ord' '-'
      || c == ord' '_'
    = BS.word8 c
    | otherwise
    = BS.asciiChar '%' <> BS.word8 x <> BS.word8 y
   where
    (x, y) = hex c

hex :: Word8 -> (Word8, Word8)
hex c = (hex1 x, hex1 y)
  where
    (x, y) = c `divMod` 16
    hex1 i
      | i < 10    = ord' '0' + i
      | otherwise = ord' 'A' + i - 10

ord' :: Char -> Word8
ord' = fromIntegral . ord

magnet :: Magnet -> Text
magnet (MkMagnet b) = decodeUtf8 . BS.builderBytes $ "magnet:?" <> b

xt :: SHA1 -> Magnet
xt hash = MkMagnet $ "xt=btih:" <> hexes
  where
    hexes = foldMap each $ unSHA1 hash
    each = (\(x, y) -> BS.word8 x <> BS.word8 y) . hex

dn :: Text -> Magnet
dn = ("dn" =:)

tr :: Text -> Magnet
tr = ("tr" =:)
