module MetaFile where

import           ClassyPrelude.Yesod hiding (fileSize)

import           Data.List           (foldl', foldr1)
import           Data.Maybe          (listToMaybe, mapMaybe)

import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.List.NonEmpty  as NE (nonEmpty)
import qualified Data.Text.Encoding  as TE


import           Util.BEncode        (BValue (..))
import qualified Util.BEncode        as B
import qualified Util.Hash           as H
import           Util.Utility

type Dictionary = [(ByteString, BValue)]

data MetaFile = MetaFile
    { info     :: Info
    , announce :: [Text]
    }

data Info = Info
    { infoHash :: H.SHA1
    , size     :: Int
    , fileTree :: FileList
    }

type FileList = Either Directory File

data Directory = Directory
    { dirName :: FilePath
    , files   :: [File]
    , dirs    :: [Directory]
    }

data File = File
    { fileName :: Text
    , fileSize :: Int
    }

decodeUtf8M :: ByteString -> Maybe Text
decodeUtf8M = eitherToMaybe . TE.decodeUtf8'

parseMeta :: BValue -> Maybe MetaFile
parseMeta bvalue = do
  dict <- B.dictionary bvalue
  info <- infoM dict
  pure $ MetaFile info (announces dict)

announces :: Dictionary -> [Text]
announces bdict = concat $ lots <|> one
 where
  lots :: Maybe [Text]
  lots = do
    lls <- lookup "announce-list" bdict >>= B.list
    let bvals :: [BValue]
        bvals = concat $ mapMaybe B.list lls

        urls :: [Text]
        urls = mapMaybe (decodeUtf8M <=< B.byteString) bvals
    pure urls

  one :: Maybe [Text]
  one = do
    urlc <- lookup "announce" bdict >>= B.byteString
    url  <- decodeUtf8M urlc
    pure [url]

infoM :: Dictionary -> Maybe Info
infoM bvals = do
  fileTree <- fileModeM bvals
  let size = calculateSize fileTree
      infoHash = calculateHash bvals
  pure Info {..}
  where
   fileModeM :: Dictionary -> Maybe FileList
   fileModeM bv = singleFileM bv

calculateHash :: Dictionary -> H.SHA1
calculateHash = H.hash . B.compose . B.BDictionary

calculateSize :: FileList -> Int
calculateSize = \case
  Right file -> fileSize file
  Left dir   -> sum (fileSize <$> files dir) + sum (calculateSize . Left <$> dirs dir)

singleFileM :: Dictionary -> Maybe FileList
singleFileM dict = Right <$> singleM dict

singleM :: Dictionary -> Maybe File
singleM dict = do
  fileName <- nameM dict >>= decodeUtf8M
  fileSize <- sizeM dict
  pure File {..}

nameM :: Dictionary -> Maybe ByteString
nameM = B.byteString <=< lookup "name"

sizeM :: Dictionary -> Maybe Int
sizeM = B.int <=< lookup "length"

multiFileM :: Dictionary -> Maybe FileList
multiFileM dict = fmap Right $ do
  name <- nameM dict
  files <- filesM dict
  undefined

-- multiM :: Dictionary -> Maybe (NonEmpty (FilePath,Int))
-- multiM = pathsM <=< filesM

filesM :: Dictionary -> Maybe [Dictionary]
filesM = traverse B.dictionary <=< B.list <=< lookup "files"

-- pathsM :: [Dictionary]-> Maybe [(FilePath, Int)]
-- pathsM = mapMaybe ((,) <<$>> pathM <<*>> sizeM)



-- -- -- FilePath is represented as a list of directories with a filename at the end
-- -- pathM :: Dictionary -> Maybe FilePath
-- -- pathM
-- --     =   fmap (foldr1 (</>))
-- --     .   (fmap . fmap) (unpack . decodeUtf8)
-- --     .   traverse B.byteString
-- --     <=< NE.nonEmpty
-- --     <=< B.list
-- --     <=< lookup "path"

