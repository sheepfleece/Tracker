module Tracker.Types where

type Failure = Text
type ResponseHTTP = Either Failure Response

data Response = Response
    { warning     :: Text
    , interval    :: Int
    , minInterval :: Maybe Int
    , trackerID   :: Maybe ByteString
    , complete    :: Int
    , incomplete  :: Int
    , peers       :: Either Dictionary Binary
    }

data Dictionary = [(ByteString, 
