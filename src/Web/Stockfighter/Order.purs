module Web.Stockfighter.Order
  ( BuyRequest(..)
  , SellRequest(..)
  ) where

type OrderRequest =
  { account :: String
  , venue :: String
  , stock :: String
  , price :: Int
  , qty :: Int
  , orderType :: OrderType
  }

newtype SellRequest = SellRequest OrderRequest
newtype BuyRequest = BuyRequest OrderRequest

data OrderDirection = Buy | Sell

data OrderType = Limit | Market | FOK | IOC

typeToString :: OrderType -> String
typeToString Limit = "limit"
typeToString Market = "market"
typeToString FOK = "fill-or-kill"
typeToString IOC = "ioc"
