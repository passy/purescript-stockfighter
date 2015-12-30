module Web.Stockfighter
  ( mkClient
  , module Heartbeat
  , module Orderbook
  )

where

import Prelude

import Web.Stockfighter.Types (StockfighterClient(..))

import qualified Web.Stockfighter.Heartbeat as Heartbeat
import qualified Web.Stockfighter.Orderbook as Orderbook

mkClient :: String -> StockfighterClient
mkClient apiKey = StockfighterClient
  { apiKey: apiKey
  , endpoint: "https://api.stockfighter.io"
  }
