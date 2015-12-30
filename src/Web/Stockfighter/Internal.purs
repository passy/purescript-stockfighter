module Web.Stockfighter.Internal
  ( mkURL
  , authedHeaders
  , authedGet
  , getJSON
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Network.HTTP.Affjax (Affjax(), AJAX(), URL(), get, affjax, defaultRequest)
import Network.HTTP.Affjax.Response (Respondable, ResponseType(..), fromResponse)
import Network.HTTP.Method (Method(GET))
import Network.HTTP.RequestHeader (RequestHeader(RequestHeader))
import Data.Foreign.Class (IsForeign, read, readProp)
import Control.Monad.Aff
import Network.HTTP.MimeType.Common (applicationJSON)
import Data.Generic (Generic, gShow)

import Web.Stockfighter.Types (StockfighterClient(..))

mkURL :: StockfighterClient -> URL -> URL
mkURL (StockfighterClient c) meth =
  c.endpoint ++ "/ob/api/" ++ meth

authedHeaders :: StockfighterClient -> Array RequestHeader
authedHeaders (StockfighterClient c) =
  [ RequestHeader "X-Starfighter-Authorization" c.apiKey ]

authedGet :: forall e a. (Respondable a) => StockfighterClient -> URL -> Affjax e a
authedGet c url =
  affjax $ defaultRequest { url = url
                          , method = GET
                          , headers = authedHeaders c
                          }

getJSON :: forall eff b. (IsForeign b, Respondable b) =>
  StockfighterClient ->
  String ->
  Aff (ajax :: AJAX | eff) b
getJSON client meth = do
  { response: response } <- authedGet client $ mkURL client meth
  liftEither <<< fromResponse $ response

  where
    -- liftEither :: forall a a1 c m. (Show a, MonadError c m) => Either a a1 -> m a1
    liftEither = either (throwError <<< error <<< show) return
