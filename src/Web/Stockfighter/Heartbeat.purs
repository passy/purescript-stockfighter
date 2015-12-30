module Web.Stockfighter.Heartbeat
  ( HeartbeatResponse()
  , heartbeat
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

import Web.Stockfighter.Internal (getJSON)
import Web.Stockfighter.Types (StockfighterClient())

newtype HeartbeatResponse = HeartbeatResponse { ok :: Boolean }

derive instance genericHeartbeatResponse :: Generic HeartbeatResponse

instance showHeartbeatResponse :: Show HeartbeatResponse where
  show = gShow

instance isForeignHeartbeatResponse :: IsForeign HeartbeatResponse where
  read o = map HeartbeatResponse $ { ok: _ }
             <$> readProp "ok" o

instance respondableHeartbeatResponse :: Respondable HeartbeatResponse where
  responseType = Tuple (Just applicationJSON) JSONResponse
  fromResponse = read

heartbeat :: forall eff. StockfighterClient -> Aff (ajax :: AJAX | eff) HeartbeatResponse
heartbeat client = getJSON client "heartbeat"
