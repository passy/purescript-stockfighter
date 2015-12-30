module Web.Stockfighter
  ( mkClient
  , module Heartbeat
  )

where

import Prelude

import Web.Stockfighter.Types (StockfighterClient(..))

import qualified Web.Stockfighter.Heartbeat as Heartbeat

mkClient :: String -> StockfighterClient
mkClient apiKey = StockfighterClient
  { apiKey: apiKey
  , endpoint: "https://api.stockfighter.io"
  }
