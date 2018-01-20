{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Server
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Presentation.Types


startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server


server :: Server PresentationAPI
server = return users

