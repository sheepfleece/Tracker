module Announce.Response where

import           ClassyPrelude.Yesod hiding (Response)
import           Network.Socket

import           Announce.Request

import qualified Data.Text.Encoding  as T
import           Util.BEncode


generalResponse :: Int -> Int -> [ByteString] -> BValue
generalResponse = response wait atLeastWait
  where
    wait = 30 * 60
    atLeastWait = 30 -- seconds

response :: Int -> Int -> Int -> Int -> [ByteString] -> BValue
response interval min_int complete_num incomplete_num peers = BDictionary
  [ ("interval", BInteger interval)
  , ("min interval", BInteger min_int)
  , ("complete", BInteger complete_num)
  , ("incomplete", BInteger incomplete_num)
  , ("peers", BString (mconcat peers))
  ]

encode :: BValue -> Text
encode = T.decodeUtf8 . compose

berror :: ByteString -> BValue
berror str = BDictionary [("failure reason", BString str)]
