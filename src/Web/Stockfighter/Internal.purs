module Web.Stockfighter.Internal
  ( ForeignDate()
  , unForeignDate
  , mkURL
  , authedHeaders
  , authedGet
  , getJSON
  ) where

import Prelude

import Control.Monad.Aff
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Error.Class (throwError)
import Data.Either (either, Either(Left))
import Data.Foreign.Class (IsForeign)
import Data.Foreign (F(), Foreign(), ForeignError(..), readString, tagOf)
import Data.Maybe (maybe)
import Network.HTTP.Affjax (Affjax(), AJAX(), URL(), affjax, defaultRequest)
import Network.HTTP.Affjax.Response (Respondable, fromResponse)
import Network.HTTP.Method (Method(GET))
import Network.HTTP.RequestHeader (RequestHeader(RequestHeader))

import qualified Data.Date as D
import Web.Stockfighter.Types (StockfighterClient(..))

newtype ForeignDate = ForeignDate D.Date

unForeignDate :: ForeignDate -> D.Date
unForeignDate (ForeignDate d) = d

instance isForeignForeignDate :: IsForeign ForeignDate where
  read = readDate

readDate :: Foreign -> F ForeignDate
readDate o =
    readString o >>= fromDateStr <#> ForeignDate
    where
      fromDateStr :: String -> F D.Date
      fromDateStr = maybe error pure <<< D.fromString

      error :: F D.Date
      error = Left $ TypeMismatch "Date" (tagOf o)

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
