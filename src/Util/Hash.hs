module Util.Hash (SHA1, hash, mySQLHash, sha1) where

import           ClassyPrelude.Yesod                hiding (hash)
import qualified Crypto.Hash.SHA1                   as C
import           Database.MySQL.Protocol.MySQLValue

newtype SHA1 = MkSHA1 { unSHA1 :: ByteString }
  deriving (Show, Eq)

hash :: ByteString -> SHA1
hash = MkSHA1 . C.hash

mySQLHash :: SHA1 -> MySQLValue
mySQLHash = MySQLBytes . unSHA1

sha1 :: ByteString -> Maybe SHA1
sha1 str
  | length str == 20 = Just $ MkSHA1 str
  | otherwise = Nothing

