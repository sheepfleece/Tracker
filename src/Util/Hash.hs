module Util.Hash (SHA1, hash, mySQLHash, parseHash, unSHA1) where

import           ClassyPrelude.Yesod                hiding (hash)
import qualified Crypto.Hash.SHA1                   as C
import           Database.MySQL.Protocol.MySQLValue

import qualified Util.BEncode                       as B

newtype SHA1 = MkSHA1 { unSHA1 :: ByteString }
  deriving (Show, Eq)

hash :: ByteString -> SHA1
hash = MkSHA1 . C.hash

mySQLHash :: SHA1 -> MySQLValue
mySQLHash = MySQLBytes . unSHA1

parseHash :: ByteString -> Maybe SHA1
parseHash str
  | length str == 20 = Just $ MkSHA1 str
  | otherwise = Nothing

