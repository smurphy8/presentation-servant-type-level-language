{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeApplications #-}
module Presentation.Types
    ( User(..)
    , PresentationAPI
    , api
    , users) where

import Data.Aeson
import Data.Aeson.TH
import Data.Proxy
import Servant.API

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type PresentationAPI = "users" :> Get '[JSON] [User]




api :: Proxy PresentationAPI
api = Proxy 


users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
