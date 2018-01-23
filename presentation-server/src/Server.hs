{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{- |
Module      : Server
Description : An example servant server 
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy


Comma Separated value list put into a servant server as an example of
using servant stuff 


-}
module Server
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Presentation.Types
import Servant
import Control.Applicative
import Data.Time 
import Text.Trifecta
import Data.Aeson
import Control.Monad.IO.Class




--------------------------------------------------
-- Parse CSV of Games
--------------------------------------------------

parseDate :: Parser UTCTime
parseDate = do
  let parseParts p = integer  <* p
  day <- fromIntegral <$>  (parseParts (char '/'))
  month <- fromIntegral <$>  parseParts (char '/')
  year <- parseParts comma
  return $ UTCTime (fromGregorian year month day ) 0



parseGameResult :: Parser GameResult
parseGameResult = ((char 'H' *> pure Home)  <|>
                   (char 'D' *> pure Draw)  <|> 
                   (char 'A' *> pure Away)) <* comma
 

parseReferee :: Parser Referee
parseReferee = do
  firstInitial <- anyChar
  _ <- space
  lastName <- manyTill alphaNum newline
  return $ Referee firstInitial lastName


validTeamNameCharacters :: Parser Char
validTeamNameCharacters = alphaNum <|> space




parseGameLine :: Parser Game
parseGameLine = do
  date <- GameDate <$> parseDate
  homeTeam <- (Team <$> manyTill validTeamNameCharacters comma)
  awayTeam <- (Team <$> manyTill validTeamNameCharacters comma)
  homeScore <- (Score . fromIntegral <$> integer <* comma)
  awayScore <- (Score . fromIntegral <$> integer <* comma)
  gameResult <- parseGameResult
  ref <- parseReferee
  return (Game date homeTeam homeScore awayTeam awayScore ref gameResult)





headerItem :: Parser Char
headerItem = alphaNum <|> comma

header :: Parser String
header = manyTill headerItem newline

parseGameFile :: Parser [Game]
parseGameFile = header *>   rest
 where
   rest = manyTill parseGameLine eof 

parseGames :: IO (Maybe _)
parseGames = parseFromFile parseGameFile  "soccer_games.csv" 


--------------------------------------------------
-- Application Setup 
--------------------------------------------------

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve listApi gameServer


--------------------------------------------------
-- Server Handling
--------------------------------------------------

server :: Server PresentationAPI
server = return users


gameServer :: Server ListAPI 
gameServer = listGames

listGames :: Handler [Game]
listGames = do maybeGames  <- liftIO parseGames
               maybe (return []) return maybeGames

