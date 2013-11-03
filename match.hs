{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

data Ticker =
  Ticker { last  :: String
         , high  :: String
         , low   :: String
         , volume :: String
         , bid :: String
         , ask :: String
           } deriving Show

instance FromJSON Ticker where
 parseJSON (Object v) =
    Ticker <$> v .: "last"
           <*> v .: "high"
           <*> v .: "low"
           <*> v .: "volume"
           <*> v .: "bid"
           <*> v .: "ask"
parseJSON _ = mzero

instance ToJSON Ticker where
 toJSON (Ticker last high low volume bid ask) =
    object [ "last"   .= last
           , "high"   .= high
           , "low"    .= low
           , "volume" .= volume
           , "bid"    .= bid
           , "ask"    .= ask
           ]

jsonURL :: String
jsonURL = "https://www.bitstamp.net/api/ticker/"

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

main :: IO ()
main = do
 -- Get JSON data and decode it
    d <- (eitherDecode <$> getJSON) :: IO (Either String Ticker)
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
    case d of
        Left err -> putStrLn err
        Right ps -> print ps
