module Main where

import Yesod 

data Hello = Hello

-- routes
mkYesod "Hello" [parseRoutes|
/ HomeR GET
-- route listening on / and answers to GET requests
-- R stands for a resourse
|]

instance Yesod Hello

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello!|]

-- Hamlet is the default template engine.
main :: IO ()
main = warp 3000 Hello


