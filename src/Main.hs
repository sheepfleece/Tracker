module Main where

import           Control.Concurrent
import           Control.Exception      (try)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BC
import           Data.Coerce
import           System.Exit            (die)

import           ClassyPrelude.Yesod    hiding (try)

import qualified Database.MySQL.Base    as My
import           Network.Socket
import           Network.Wai            as Wai

import qualified Announce.Request       as Req
import qualified Announce.Response      as Res
import           Database.Tracker
import           Util.BEncode           (BValue)

data App = App
    { dbConn :: My.MySQLConn
    }

connectInfo :: My.ConnectInfo
connectInfo = My.defaultConnectInfoMB4
  { My.ciUser     = "root"
  , My.ciDatabase = "torrent"
  , My.ciPassword = ""
  }

instance Yesod App

mkYesod "App" [parseRoutes|
/         HomeR    GET
/announce AnnounceR GET
|]

getHomeR :: Handler Html
getHomeR = error "to implement"

getAnnounceR :: Handler Text
getAnnounceR = do
  query <- Wai.queryString <$> waiRequest
  host  <- Wai.remoteHost <$> waiRequest

  Res.encode <$> respondTo host query

respondTo :: SockAddr -> Query -> Handler BValue
respondTo host query = case Req.parse host query of
  Left reason -> pure $ Res.berror reason
  Right announce -> do
    App {..} <- getYesod
    mres <- liftIO . try @RequestException $
      runReaderT (withPeer >> peers) (dbConn, announce)

    pure $ case mres of
      Left err -> Res.berror $
        (BC.pack . show) err
      Right ((wait, atLeast), peers) ->
        Res.generalResponse wait atLeast peers

main :: IO ()
main = do
  token <- My.connect connectInfo
  _ <- forkIO (purge token (60 * 60 * (10 ^ 6))) -- microseconds
  warp 4000 (App token)

