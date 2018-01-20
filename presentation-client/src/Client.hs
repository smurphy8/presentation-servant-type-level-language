{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Client ( ) where

import Data.Aeson
import Data.Aeson.TH
import Servant.Client
import Presentation.Types

