module Web.Stockfighter
  ( mkClient
  , heartbeat
  , HeartbeatResponse(..)
  , StockfighterClient(..)
  )

where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Control.Bind ((>=>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Network.HTTP.Affjax (Affjax(), AJAX(), URL(), get)
import Network.HTTP.Affjax.Response (Respondable, ResponseType(..), fromResponse)
import Data.Foreign.Class (IsForeign, read, readProp)
import Control.Error.Util (hush)
import Control.Monad.Aff
import Network.HTTP.MimeType.Common (applicationJSON)
import Data.Generic (Generic, gShow)

newtype StockfighterClient = StockfighterClient
  { apiKey :: String
  , endpoint :: URL
  }

newtype HeartbeatResponse = HeartbeatResponse { ok :: Boolean }

derive instance genericHeartbeatResponse :: Generic HeartbeatResponse

instance showHeartbeatResponse :: Show HeartbeatResponse where
  show = gShow

mkClient :: String -> StockfighterClient
mkClient apiKey = StockfighterClient
  { apiKey: apiKey
  , endpoint: "https://api.stockfighter.io"
  }

mkURL :: StockfighterClient -> URL -> URL
mkURL (StockfighterClient c) meth =
  c.endpoint ++ "/ob/api/" ++ meth

heartbeat :: forall eff. StockfighterClient -> Aff (ajax :: AJAX | eff) (Maybe HeartbeatResponse)
heartbeat client = do
  { response: response } <- get (mkURL client "heartbeat")
  pure <<< hush <<< fromResponse $ response

  where
    eitherToMaybe = either (const Nothing) Just

instance isForeignHeartbeatResponse :: IsForeign HeartbeatResponse where
  read o = map HeartbeatResponse $ { ok: _ }
             <$> readProp "ok" o

instance respondableHeartbeatResponse :: Respondable HeartbeatResponse where
  responseType = Tuple (Just applicationJSON) JSONResponse
  fromResponse = read
