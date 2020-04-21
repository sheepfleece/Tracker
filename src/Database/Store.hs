module Database.Store where


import           ClassyPrelude.Yesod
import           Data.Maybe
import qualified Database.MySQL.Base as My

import           Text.Heredoc
import qualified Util.BEncode        as B
import           Util.Hash
import           Util.Magnet


data Torrent = Torrent
    { filename :: Text
    , contents :: ByteString
    , magnet   :: Text
    , hash     :: SHA1
    }


-- torrent :: Text -> ByteString -> Either Text Torrent
-- torrent name file =
--   case B.parse file of
--     Left v -> Left $ pack v
--     Right bval -> Right
--       Torrent name bdata (fromJust (infoHash file))

-- iTorrent :: My.Query
-- iTorrent = [here|
-- INSERT Torrent (name, torrent, SHA1) VALUES (?, ?, ?)
-- |]


