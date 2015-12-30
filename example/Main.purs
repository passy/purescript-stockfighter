-- Run with:
-- pulp build -I example && pulp run -m Example.Main
module Example.Main where

import Prelude
import Control.Monad.Eff.Console (print)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (launchAff)

import qualified Web.Stockfighter as S

main = launchAff $ do
  let client = S.mkClient "8346a073e7292aabb3e6d6e2b3b7125a618f84f3"
  res <- S.heartbeat client

  liftEff $ print res
