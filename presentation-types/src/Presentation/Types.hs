{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeApplications #-}
module Presentation.Types
    ( User(..)
    , listApi
    , ListAPI
    , GameDate(..)
    , Team (..)
    , Score (..)
    , Referee (..)
    , Game(..)
    , GameResult (..)
    , PresentationAPI
    , api
    , users) where

import Data.Aeson
import Data.Aeson.TH
import Data.Proxy
import Servant.API
import Data.Time


newtype GameDate= GameDate{unGameDate:: UTCTime }
$(deriveJSON defaultOptions ''GameDate)


newtype Team = Team {unTeam :: String}
$(deriveJSON defaultOptions ''Team)

newtype Score = Score {unScore :: Int}
$(deriveJSON defaultOptions ''Score)


data Referee = Referee
  { _initial :: Char,
    _lastName :: String}

$(deriveJSON defaultOptions ''Referee)

data GameResult = Home | Away | Draw 

$(deriveJSON defaultOptions ''GameResult)

data Game = Game
 {
   _date :: GameDate,
   _homeTeam :: Team,
   _homeScore :: Score,
   _awayTeam :: Team,
   _awayScore :: Score,
   _referee :: Referee,
   _result :: GameResult
 }


$(deriveJSON defaultOptions ''Game)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type PresentationAPI = "users" :> Get '[JSON] [User]

type ListAPI = "games" :> Get '[JSON] [Game]

listApi :: Proxy ListAPI
listApi = Proxy

api :: Proxy PresentationAPI
api = Proxy 


users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]


