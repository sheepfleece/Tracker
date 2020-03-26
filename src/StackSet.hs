module StackSet where

data StackSet a = StackSet
  { up    :: [a] -- right
  , down  :: [a] -- left
  , focus :: a
  } deriving (Functor)

data Thumbnail s a = Thumbnail
  { screen :: s
  , images :: StackSet a
  }


