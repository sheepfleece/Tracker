module Database.Store where


import           ClassyPrelude.Yesod
import           Data.Maybe
import qualified Database.MySQL.Base as My
import           Text.Heredoc

import           Config
import qualified MetaFile            as M
import qualified Util.BEncode        as B
import qualified Util.Hash           as H
import           Util.Magnet

data Torrent = Torrent
    { filename   :: Text
    , contents   :: ByteString
    , magnetLink :: Text
    , metaFile   :: M.MetaFile
    }

torrent :: Text -> Text -> ByteString -> Either Text Torrent
torrent ourUrl filename contents = case B.parse contents of
  Left _ -> Left "Corrupted .torrent"
  Right bvalue -> case parseTorrent ourUrl filename contents bvalue of
    Nothing -> Left "Corrupted .torrent"
    Just t  -> Right t

insertTracker :: ByteString -> ByteString
insertTracker = undefined

parseTorrent :: Text -> Text -> ByteString -> B.BValue -> Maybe Torrent
parseTorrent ourUrl filename contents' bvalue = do
  metaFile <- M.parseMeta bvalue
  let urls = ourUrl : filter (/= ourUrl) (M.announce metaFile)
      hash = M.infoHash . M.info $ metaFile
      magnetLink = createMagnetLink hash filename urls
      contents = insertTracker contents'
  pure Torrent {..}

createMagnetLink :: H.SHA1 -> Text -> [Text] -> Text
createMagnetLink hash name urls =
  magnet $ xt hash <> dn name <> concatMap tr urls


iTorrent :: My.Query
iTorrent = [here|
INSERT Torrent (name, torrent, SHA1) VALUES (?, ?, ?)
|]


