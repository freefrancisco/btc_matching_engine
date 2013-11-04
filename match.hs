{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

data Order =
    Order { price :: Double
          , amt   :: Double
          } deriving Show

data OrderBook =
    OrderBook { timestamp  :: Int
              , bids  :: [Order]
              , asks   :: [Order]
              } deriving Show

instance FromJSON OrderBook where
    parseJSON (Object v) =
        OrderBook <$> liftM read (v .: "timestamp")
                  <*> liftM processOrderList (v .: "bids")
                  <*> liftM processOrderList (v .: "asks")
    parseJSON _ = mzero

processOrderList :: [[String]] -> [Order]
processOrderList = fmap orderFromArr

orderFromArr :: [String] -> Order
orderFromArr (x:y:[]) = Order (read x) (read y)
orderFromArr _        = Order 0 0

data Ticker =
  Ticker { last   :: Double
         , high   :: Double
         , low    :: Double
         , volume :: Double
         , bid    :: Double
         , ask    :: Double
         } deriving Show

instance FromJSON Ticker where
    parseJSON (Object v) =
        Ticker <$> liftM read (v .: "last")
               <*> liftM read (v .: "high")
               <*> liftM read (v .: "low")
               <*> liftM read (v .: "volume")
               <*> liftM read (v .: "bid")
               <*> liftM read (v .: "ask")
    parseJSON _ = mzero

jsonTickerURL :: String
jsonTickerURL = "https://www.bitstamp.net/api/ticker/"

jsonOrderBookURL :: String
jsonOrderBookURL = "https://www.bitstamp.net/api/order_book/"

tickerJSON :: IO B.ByteString
tickerJSON = simpleHttp jsonTickerURL

orderBookJSON :: IO B.ByteString
orderBookJSON = simpleHttp jsonOrderBookURL


main :: IO ()
main = do
    d <- (eitherDecode <$> orderBookJSON) :: IO (Either String OrderBook)

--    d <- (eitherDecode <$> tickerJSON) :: IO (Either String Ticker)
    case d of
        Left err -> putStrLn err
        Right ps -> print ps
