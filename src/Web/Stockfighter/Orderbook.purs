module Web.Stockfighter.Orderbook
  ( OrderbookRequest()
  , OrderbookOrder(..)
  , OrderbookResponse(..)
  , orderbook
  ) where

import Prelude

import Control.Monad.Aff (Aff())
import Data.Foreign.Class (IsForeign, read, readProp)
import Data.Generic (Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX())
import Network.HTTP.Affjax.Response (Respondable, ResponseType(..))
import Network.HTTP.MimeType.Common (applicationJSON)
import Data.Date (Date())

import Web.Stockfighter.Internal (getJSON, unForeignDate)
import Web.Stockfighter.Types (StockfighterClient())

newtype OrderbookOrder = OrderbookOrder
  { price :: Int
  , qty :: Int
  , isBuy :: Boolean
  }

derive instance genericOrderbookOrder :: Generic OrderbookOrder

instance showOrderbookOrder :: Show OrderbookOrder where
  show = gShow

newtype OrderbookResponse = OrderbookResponse
  { venue :: String
  , symbol :: String
  , bids :: Array OrderbookOrder
  , asks :: Array OrderbookOrder
  , ts :: Date
  }

instance showOrderbookResponse :: Show OrderbookResponse where
  show (OrderbookResponse r) =
    "OrderbookResponse { venue = " <> show r.venue <> ", "
                      <> "symbol = " <> show r.symbol <> ", "
                      <> "bids = " <> show r.bids <> ", "
                      <> "asks = " <> show r.asks <> ", "
                      <> "ts = " <> show r.ts <> ", "
                      <> "}"

type OrderbookRequest = { venue :: String, stock :: String }

instance isForeignOrderbookOrder :: IsForeign OrderbookOrder where
  read o = map OrderbookOrder $ { price: _, qty: _, isBuy: _ }
             <$> readProp "price" o
             <*> readProp "qty" o
             <*> readProp "isBuy" o

instance isForeignOrderbookResponse :: IsForeign OrderbookResponse where
  read o = map OrderbookResponse
              $ { venue: _, symbol: _, bids: _, asks: _, ts: _ }
             <$> readProp "venue" o
             <*> readProp "symbol" o
             <*> readProp "bids" o
             <*> readProp "asks" o
             <*> (readProp "ts" o <#> unForeignDate)

instance respondableOrderbookResponse :: Respondable OrderbookResponse where
  responseType = Tuple (Just applicationJSON) JSONResponse
  fromResponse = read

orderbook :: forall eff.
  StockfighterClient ->
  OrderbookRequest ->
  Aff (ajax :: AJAX | eff) OrderbookResponse
orderbook client req = getJSON client $ "venues/" <> req.venue <> "/stocks/" <> req.stock
