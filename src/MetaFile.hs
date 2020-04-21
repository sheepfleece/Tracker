module MetaFile where

import           ClassyPrelude.Yesod hiding (fileSize)
import           Data.Coerce
import           Data.Monoid

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

data MetaFile a = MetaFile
    { info     :: Info a
    , announce :: [Text]
    }

data Info a = Info
    { infoHash :: H.SHA1
    , size     :: Int
    , fileTree :: FileList a
    }

type FileList a = Either (Directory a) (File a)

data Directory a = Directory
    { dirName :: a
    , files   :: [File a]
    , dirs    :: [Directory a]
    }
    deriving (Functor, Foldable, Traversable)

data File a = File
    { fileName :: a
    , fileSize :: Int
    }
    deriving (Functor, Foldable, Traversable)

decodeUtf8M :: ByteString -> Maybe Text
decodeUtf8M = eitherToMaybe . TE.decodeUtf8'

parseMeta :: BValue -> Maybe (MetaFile Text)
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

infoM :: Dictionary -> Maybe (Info Text)
infoM bvals = do
  fileTree <- fileModeM bvals
  let size = calculateSize fileTree
      infoHash = calculateHash bvals
  pure Info {..}
  where
   fileModeM :: Dictionary -> Maybe (FileList Text)
   fileModeM bv = singleFileM bv

calculateHash :: Dictionary -> H.SHA1
calculateHash = H.hash . B.compose . B.BDictionary

calculateSize :: FileList a -> Int
calculateSize = \case
  Right file -> fileSize file
  Left dir   -> sum (fileSize <$> files dir) + sum (calculateSize . Left <$> dirs dir)

singleFileM :: Dictionary -> Maybe (FileList Text)
singleFileM dict = Right <$> singleM dict

singleM :: Dictionary -> Maybe (File Text)
singleM dict = do
  fileName <- nameM dict >>= decodeUtf8M
  fileSize <- sizeM dict
  pure File {..}

nameM :: Dictionary -> Maybe ByteString
nameM = B.byteString <=< lookup "name"

sizeM :: Dictionary -> Maybe Int
sizeM = B.int <=< lookup "length"

via :: forall a d b c . (Coercible a b, Coercible c d) => (a -> c) -> b -> d
via f = coerce . f . coerce

multiFileM :: Dictionary -> Maybe (FileList Text)
multiFileM dict = fmap Left $ do
  name <- nameM dict
  files <- filesM dict
  let builder :: Directory ByteString -> Directory ByteString
      builder = via @[Endo (Directory ByteString)] concat (addFile <$> files)

      dirBS :: Directory ByteString
      dirBS = builder (Directory name [] [])

  traverse decodeUtf8M dirBS

filesM :: Dictionary -> Maybe [(Pieces, Int)]
filesM dict = do
  bvals <- lookup "files" dict >>= B.list
  fdicts <- traverse B.dictionary bvals
  traverse parseFile fdicts

type Pieces = [ByteString]

addFile :: (Pieces, Int) -> Directory ByteString -> Directory ByteString
addFile ([fileName], fileSize) dir = dir { files = File {..} : files dir }


parseFile :: Dictionary -> Maybe (Pieces, Int)
parseFile dict = do
  path <- pathM dict
  size <- sizeM dict
  pure (path, size)


pathM :: Dictionary -> Maybe Pieces
pathM dict = do
    bvals <- lookup "path" dict >>= B.list
    traverse B.byteString bvals



