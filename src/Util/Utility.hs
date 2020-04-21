module Util.Utility
  ( (<<*>>)
  , (<<$>>)
  , eitherToMaybe
  , chunksOf
  )
where

import           ClassyPrelude.Yesod

-- Applicatives are closed under composition
(<<*>>)
  :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)
infixl 4 <<*>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
infixl 4 <<$>>

-- splits ByteString into chunks of the same size
chunksOf :: Int -> ByteString -> [ByteString]
chunksOf i | i <= 0 = error "Should be positive"
chunksOf i = repeatedly (splitAt i)

repeatedly
  :: (ByteString -> (ByteString, ByteString)) -> ByteString -> [ByteString]
repeatedly _ bs | null bs = []
repeatedly f as              = b : repeatedly f as' where (b, as') = f as


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just
