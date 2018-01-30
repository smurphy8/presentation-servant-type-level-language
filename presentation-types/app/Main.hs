{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- Orphan Instances here for swagger doc generation 
module Main where

import Presentation.Types

import Servant.Swagger
import Data.Swagger
import Servant.Swagger.UI
import Data.Proxy
import Servant.Server
import Network.Wai.Handler.Warp

type WithSwaggerAPI = SwaggerSchemaUI "swagger-ui" "swager.json" 

instance ToSchema Game 
instance ToSchema GameDate
instance ToSchema GameResult
instance ToSchema Team
instance ToSchema Score
instance ToSchema Referee


server :: Server WithSwaggerAPI
server = swaggerSchemaUIServer (toSwagger (Proxy :: Proxy ListAPI))


{- |
/swagger.json
/swagger-ui
/swagger-ui/index.html
/swagger-ui/...
-}



main :: IO ()
main = run 8080 app
  where
    app = serve (Proxy :: Proxy WithSwaggerAPI)  server
