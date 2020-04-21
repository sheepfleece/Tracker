module Database.Tracker where

import           ClassyPrelude.Yesod
import           Control.Concurrent
import           Data.Bits
import           Data.Word
import qualified Prelude                            as P

import           Control.Exception
import           Data.Coerce
import qualified Data.List                          as L
import           Database.MySQL.Base                (MySQLConn)
import qualified Database.MySQL.Base                as My
import           Database.MySQL.Protocol.MySQLValue
import qualified System.IO.Streams                  as S

import           Text.Heredoc

import           Announce
import           Network.Socket
import           Util.Hash

type SQLQuery = ReaderT (MySQLConn, Announce) IO

data RequestException = CannotFindTorrent

instance Show RequestException where
  showsPrec _ = \case
    CannotFindTorrent -> P.showString "Unregistered torrent"

instance Exception RequestException

composeAddress :: Word32 -> Word16 -> ByteString
composeAddress ip port = pack $ word32 ip <> word16 port

word16 :: Word16 -> [Word8]
word16 w = [byte 8, byte 0]
  where
    byte n = fromIntegral $ w `shiftR` n

word32 :: Word32 -> [Word8]
word32 (hostAddressToTuple -> (x, y, z, w)) = [x, y, z, w]

consume :: S.InputStream a -> IO [a]
consume stream = do
  mx <- S.read stream
  case mx of
    Nothing -> pure []
    Just x  -> do
      xs <- consume stream
      pure (x:xs)

stripBS :: MySQLValue -> ByteString
stripBS = \case
  MySQLBytes v -> v
  _ -> error "Query failed"

stripWord :: MySQLValue -> Int
stripWord = \case
  MySQLInt32U v -> fromIntegral v
  _ -> error "Query failed"


peers :: SQLQuery ((Int, Int), [ByteString])
peers = do
  token <- asks fst
  Announce{..} <- asks snd

  liftIO $ do
    blob <- consume . snd =<<
      My.query token peersQuery [mySQLHash info_hash, MySQLInt32U numwant]
    let
      stats = giveOrTake blob
      ips   = stripBS . L.head <$> blob
    pure (stats, ips)


giveOrTake :: [[MySQLValue]] -> (Int, Int)
giveOrTake = \case
  [] -> (0, 0)
  ([_, x, y]:_) -> (stripWord x, stripWord y)
  _ -> error "Query failed"

withPeer :: SQLQuery ()
withPeer = do
  e <- asks (event . snd)
  case e of
    Stopped   -> deletePeer
    Completed -> completePeer
    _         -> updatePeer

completePeer :: SQLQuery ()
completePeer = do
  token <- asks fst
  Announce{..} <- asks snd
  let address = composeAddress ip port
  liftIO $ do
    _ <- My.execute token completeQuery
      [MySQLBytes address]
    pure ()

deletePeer :: SQLQuery ()
deletePeer = do
  token <- asks fst
  Announce{..} <- asks snd
  let address = composeAddress ip port
  liftIO $ do
    ls <- consume =<<
      (snd <$> My.query token idQuery [mySQLHash info_hash])
    when (null ls) (throw CannotFindTorrent)
    let tid = (L.head . L.head) ls

    ok <- My.execute token deleteQuery
      [MySQLBytes address, tid]

    P.putStrLn $ show ok
    pure ()

updatePeer :: SQLQuery ()
updatePeer = do
  token <- asks fst
  Announce{..} <- asks snd
  let address = composeAddress ip port
  liftIO $ do
    ls <- consume =<< (snd <$> My.query token idQuery [mySQLHash info_hash])
    when (null ls) (throw CannotFindTorrent)
    let tid = (L.head . L.head) ls

    -- there is a small data race. TODO
    _ <- My.execute token updateQuery
      [MySQLBytes address, tid
      , MySQLInt32U uploaded, MySQLInt32U downloaded]
    pure ()

purge :: MySQLConn -> Int -> IO ()
purge token mks = forever $ do
  _ <- My.execute_ token purgeQuery
  threadDelay mks


completeQuery :: My.Query
completeQuery = [here|
UPDATE Torrent t, Seed s
SET t.seeders=t.seeders+1, t.leechers=t.leechers-1, s.completed=1
WHERE t.id = s.FK_torrent AND s.address = ? AND s.completed = 0
|]

deleteQuery :: My.Query
deleteQuery = [here|
DELETE FROM Seed WHERE address = ? AND FK_torrent = ?
|]

updateQuery :: My.Query
updateQuery = [here|
INSERT Seed (address, FK_torrent) VALUES (?, ?)
ON DUPLICATE KEY UPDATE uploaded=?, downloaded=?
|]

purgeQuery :: My.Query
purgeQuery = [here|
DELETE FROM Seed WHERE keep_alive < (NOW() - INTERVAL 60 MINUTE)
|]

idQuery :: My.Query
idQuery = [here|
SELECT id FROM Torrent
WHERE SHA1 = ?
|]

peersQuery :: My.Query
peersQuery = [here|
SELECT s.address, t.seeders, t.leechers FROM Torrent t
INNER JOIN Seed s ON t.id = s.FK_torrent
WHERE t.SHA1 = ?
LIMIT ?
|]
