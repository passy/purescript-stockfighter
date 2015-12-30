module Web.Stockfighter.Types
  ( StockfighterClient(..)
  ) where

import Network.HTTP.Affjax (URL())

newtype StockfighterClient = StockfighterClient
  { apiKey :: String
  , endpoint :: URL
  }
