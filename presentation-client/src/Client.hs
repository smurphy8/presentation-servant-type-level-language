{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Client ( ) where

import Data.Aeson
import Data.Aeson.TH
import Servant.Client
import Servant.Common.BaseUrl
import qualified Network.HTTP.Client         as Client
import Presentation.Types
import Data.Proxy (Proxy)





api :: Proxy ListAPI
api = listApi


(getListGames) = client api


exampleGetListGames :: IO (Either ServantError [Game])
exampleGetListGames = do
  manager' <- Client.newManager Client.defaultManagerSettings
  url <- Servant.Common.BaseUrl.parseBaseUrl "http://localhost:8080"
  runClientM getListGames (ClientEnv manager' url)
  
