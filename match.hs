{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

data OrderBook =
    OrderBook { timestamp  :: String
            , bids  :: [[String]]
            , asks   :: [[String]]
            } deriving Show

instance FromJSON OrderBook where
    parseJSON (Object v) =
        OrderBook <$> v .: "timestamp"
                  <*> v .: "bids"
                  <*> v .: "asks"
    parseJSON _ = mzero

instance ToJSON OrderBook where
    toJSON (OrderBook timestamp bids asks) =
        object [ "timestamp" .= timestamp
            , "bids"      .= bids
            , "asks"      .= asks
            ]

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

jsonTickerURL :: String
jsonTickerURL = "https://www.bitstamp.net/api/ticker/"

jsonOrderBookURL :: String
jsonOrderBookURL = "https://www.bitstamp.net/api/order_book/"

getTickerJSON :: IO B.ByteString
getTickerJSON = simpleHttp jsonTickerURL

getOrderBookJSON :: IO B.ByteString
getOrderBookJSON = simpleHttp jsonOrderBookURL

main :: IO ()
main = do
    d <- (eitherDecode <$> getOrderBookJSON) :: IO (Either String OrderBook)
    case d of
        Left err -> putStrLn err
        Right ps -> print ps
