module Announce where

import           ClassyPrelude.Yesod        hiding (find)
import           Data.Attoparsec.ByteString
import qualified Data.List                  as L
import qualified Data.Text.Encoding         as T
import           Data.Word

import qualified Network.Socket             as NS
import qualified Network.Wai                as NW

import           Util.BEncode
import           Util.Hash

data Announce = Announce
    { info_hash  :: SHA1
    , port       :: !Word16
    , uploaded   :: !Word32
    , downloaded :: !Word32
    , left       :: !Word64
    , event      :: !Event
    , numwant    :: !Word32
    , ip         :: !Word32
    }

data Event = Started
    | Stopped
    | Completed
    | KeepAlive

-- Peer_id is discarded, for we can
-- use ip and port to uniquely identify our peers.
--
-- That implies we can only serve compacted messages
-- And because of this we do not support ipv6
--
-- And lastly we ignore ip field and instead use
-- one from which query was received
-- TODO. Maybe I should change it?
parse :: NS.SockAddr -> Query -> Either ByteString Announce
parse host qs = do
  ipv4 <- parseIP host
  i <- parseSHA1 =<< find "info_hash" qs
  p <- convert "port" qs
  u <- convert "uploaded" qs
  d <- convert "downloaded" qs
  l <- convert "left" qs
  e <- parseEvent $ find "event" qs
  nw <- parseNumWant qs
  pure $ Announce i p u d l e nw ipv4

convert :: Read a => ByteString -> Query -> Either ByteString a
convert param qs = do
  val <- T.decodeUtf8 <$> find param qs
  case readMay val of
    Nothing   -> Left $ "Couldn't parse value of " <> param
    Just val' -> Right val'

find :: ByteString -> Query -> Either ByteString ByteString
find param qs = case L.lookup param qs of
  Nothing  -> Left $ "Cannot find: " <> param
  Just val ->
    case val  of
      Nothing   -> Left $ param <> " should have a value"
      Just val' -> Right val'

parseSHA1 :: ByteString -> Either ByteString SHA1
parseSHA1 v = case parseHash v of
  Nothing   -> Left "Is not a valid SHA value"
  Just hash -> Right hash

parseNumWant :: Query -> Either ByteString Word32
parseNumWant qs = case convert "numwant" qs of
  Left _ -> Right defNum
  Right int ->
    if | int < 0 -> Left "Negative numwant"
       | int >= 0 && int <= 50 -> Right int
       | otherwise -> Right defNum
  where defNum = 15

parseIP :: NS.SockAddr -> Either ByteString Word32
parseIP = \case
  NS.SockAddrInet6 _ _ _ _ -> Left "As of now only ipv4 is supported."
  NS.SockAddrUnix _ -> Left "As of now only ipv4 is supported."
  NS.SockAddrInet _ ipv4 -> Right ipv4
  NS.SockAddrCan _ -> error "TODO: Deprecated"

parseEvent :: Either ByteString ByteString -> Either ByteString Event
parseEvent = \case
    Left _ -> Right KeepAlive
    Right "started" -> Right Started
    Right "stopped" -> Right Stopped
    Right "completed" -> Right Completed
    Right event -> Left $ "Expected event, but got: " <> event

{- Response -}

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

