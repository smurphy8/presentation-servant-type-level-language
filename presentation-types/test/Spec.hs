{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Servant.Aeson.GenericSpecs
import Data.Proxy
import Presentation.Types
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT
import Data.Time
import qualified Data.Time.Clock.POSIX as POSIX



main :: IO ()
main = hspec spec



instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$>
              arbitrary 

instance Arbitrary UTCTime where
  arbitrary = (POSIX.posixSecondsToUTCTime . fromInteger) <$> arbitrary

instance Arbitrary GameResult where
  arbitrary = genericArbitrary

instance Arbitrary Referee where
  arbitrary = genericArbitrary

instance Arbitrary Score where
  arbitrary = genericArbitrary

instance Arbitrary Team where
  arbitrary = genericArbitrary

instance Arbitrary GameDate where
  arbitrary = genericArbitrary


instance Arbitrary Game where
  arbitrary = genericArbitrary



spec = apiSpecs specProxy
 where
    specProxy :: Proxy ListAPI
    specProxy = Proxy
