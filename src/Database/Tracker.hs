module Database.Tracker where

import           ClassyPrelude.Yesod
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

import           Announce.Request
import           Network.Socket
import           Util.Hash
import           Util.Types

type SQLQuery = ReaderT (MySQLConn, Announce) IO

data RequestException = CannotFindTorrent
    deriving (Show)

instance Exception RequestException

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

completeQuery :: My.Query
completeQuery = [here|
UPDATE Torrent t, Seed s
SET t.seeders=t.seeders+1, t.leechers=t.leechers-1, s.completed=1
WHERE t.id = s.FK_torrent AND s.address = ? AND s.completed = 0
|]


composeAddress :: Word32 -> Word16 -> ByteString
composeAddress ip port = pack $ word32 ip <> word16 port

deletePeer :: SQLQuery ()
deletePeer = do
  token <- asks fst
  Announce{..} <- asks snd
  let address = composeAddress ip port
  liftIO $ do
    ls <- consume =<< (snd <$> My.query token idQuery [mySQLHash info_hash])
    when (null ls) (throw CannotFindTorrent)
    let tid = (L.head . L.head) ls

    ok <- My.execute token deleteQuery
      [MySQLBytes address, tid]

    P.putStrLn $ show ok
    pure ()

deleteQuery :: My.Query
deleteQuery = [here|
DELETE FROM Seed WHERE address = ? AND FK_torrent = ?
|]

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

purge :: MySQLConn -> IO ()
purge token = do
  _ <- My.execute token_ purgeQuery
  pure ()

purgeQuery :: My.Query
purgeQuery = [here|
DELETE FROM Seed WHERE keep_alive < (NOW() - INTERVAL 60 MINUTE)
|]

