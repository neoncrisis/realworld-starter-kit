{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module RealWorld.Server where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Proxy (Proxy (..))
import Network.Wai.Handler.Warp (run)
import RealWorld.App (App, runToHandler)
import RealWorld.Config (Config (..), connectDb)
import Servant.API
import Servant.Server

-- server :: Api (AsServerT (AppT IO))
server :: ServerT EmptyAPI App
server = emptyServer

runServer :: IO ()
runServer = do
  putStrLn "Listening on port 8080..."

  db <- runStdoutLoggingT $ connectDb "postgres://localhost:5432"
  let cfg = Config db

  run 8080
    . serve (Proxy @EmptyAPI)
    . hoistServer (Proxy @EmptyAPI) (runToHandler cfg)
    $ server
