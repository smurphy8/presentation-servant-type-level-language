{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

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


startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server


parseDate :: Parser UTCTime
parseDate = do
  let parseParts p = integer  <* p
  day <- fromIntegral <$>  (parseParts (char '/'))
  month <- fromIntegral <$>  parseParts (char '/')
  year <- parseParts comma
  return $ UTCTime (fromGregorian year month day ) 0



parseGameResult :: Parser GameResult
parseGameResult = ((char 'H' *> pure Home) <|>
                   (char 'D' *> pure Draw) <|> 
                  (char 'A' *> pure Away)) <* comma


parseReferee :: Parser Referee
parseReferee = do
  firstInitial <- anyChar
  _ <- space
  lastName <- manyTill alphaNum newline
  return $ Referee firstInitial lastName


validTeamNameCharacters = alphaNum <|> space

parseGameLine = do
  date <- GameDate <$> parseDate
  homeTeam <- (Team <$> manyTill validTeamNameCharacters comma)
  awayTeam <- (Team <$> manyTill validTeamNameCharacters comma)
  homeScore <- (Score . fromIntegral <$> integer <* comma)
  awayScore <- (Score . fromIntegral <$> integer <* comma)
  gameResult <- parseGameResult
  ref <- parseReferee
  return (Game date homeTeam homeScore awayTeam awayScore ref gameResult)



server :: Server PresentationAPI
server = return users


headerItem = alphaNum <|> comma

header = manyTill headerItem newline

parseGameFile = header *>   rest
 where
   rest = manyTill parseGameLine eof 



parseGames :: IO (Maybe _)
parseGames = parseFromFile parseGameFile  "soccer_games.csv" 
