{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module RealWorld.Server where

import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Control.Monad.Logger (logInfoNS, runChanLoggingT, runStdoutLoggingT, unChanLoggingT)
import Control.Monad.Logger (MonadLogger)
import Data.Generics.Labels ()
import Data.Proxy (Proxy (..))
import Lens.Micro.Platform ((^.))
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import RealWorld.Api (Api)
import RealWorld.App (runToHandler)
import RealWorld.Config (Config (..), connectDb)
import Servant.Server
import UnliftIO.Async (async)
import UnliftIO.Chan (newChan)
import UnliftIO.Exception (bracket)
import UnliftIO.Pool (destroyAllResources)



server :: ServerT Api m
server = emptyServer

loadConfig
  :: ( MonadUnliftIO m
     , MonadLogger m
     )
  => m Config
loadConfig = do
  db <- connectDb "postgres://localhost:5432"
  pure $ Config db

shutdownApp
  :: MonadUnliftIO m
  => Config
  -> m ()
shutdownApp cfg = do
  destroyAllResources $ cfg ^. #dbPool

runServer
  :: MonadUnliftIO m
  => m ()
runServer = runStdoutLoggingT $
  bracket loadConfig shutdownApp launch

  where
    launch cfg = do
      logInfoNS "INIT" "haskell-servant-realworld"
      logInfoNS "INIT" "Listening on port 8080"

      -- Create a separate thread to handle writing logs
      ch <- newChan
      _  <- async $ runStdoutLoggingT (unChanLoggingT ch)

      liftIO
        $ run 8080
        . logStdout
        . serveWithContext (Proxy @Api) EmptyContext
        . hoistServer (Proxy @Api) (runToHandler (runChanLoggingT ch) cfg)
        $ server
