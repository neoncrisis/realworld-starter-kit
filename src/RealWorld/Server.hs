{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module RealWorld.Server where

import Control.Exception.Safe (MonadThrow, throwM)
import Control.Lens ((^.))
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, MonadLogger, logInfoNS,
                             runChanLoggingT, runStdoutLoggingT, unChanLoggingT)
import Data.Generics.Labels ()
import Database.Persist.Sql (SqlBackend)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import RealWorld.Api (Api (..), UserApi (..))
import RealWorld.App (runToHandler)
import RealWorld.Config (Config (..), connectDb)
import RealWorld.Service.User (MonadUser)
import qualified RealWorld.Service.User as User
import Servant.Server
import Servant.Server.Generic
import UnliftIO.Chan (Chan, newChan)
import UnliftIO.Concurrent (ThreadId, forkIO, killThread)
import UnliftIO.Exception (bracket)
import UnliftIO.Pool (Pool, destroyAllResources)


data Init = Init
  { dbPool     :: !(Pool SqlBackend)
  , logThread  :: !ThreadId
  , logChannel :: !(Chan (Loc, LogSource, LogLevel, LogStr))
  } deriving (Generic)


-- | Allocates resources needed to run our server
initializeApp :: (MonadLogger m, MonadUnliftIO m) => m Init
initializeApp = do
  db  <- connectDb "postgres://localhost:5432"
  ch  <- newChan
  tid <- forkIO $ runStdoutLoggingT (unChanLoggingT ch)
  pure $ Init
    { dbPool     = db
    , logChannel = ch
    , logThread  = tid
    }


-- | Handles clean up of resources when the app shuts down
shutdownApp :: MonadUnliftIO m => Init -> m ()
shutdownApp i = do
  destroyAllResources $ i ^. #dbPool
  killThread $ i ^. #logThread


-- | Runs an app handler making sure to clean up allocated resources
bracketApp :: (MonadLogger m, MonadUnliftIO m) => (Init -> m a) -> m a
bracketApp =
  bracket initializeApp shutdownApp


-- | Create the WAI 'Application' representing our server
application
  :: ( MonadUser m
     , MonadThrow m
     )
  => (forall a. m a -> Handler a)
  -> Application
application handle =
    logStdout
  $ genericServeTWithContext handle server ctx

  where
    ctx = EmptyContext


runServer
  :: ( MonadThrow m
     , MonadUnliftIO m
     )
  => m ()
runServer = runStdoutLoggingT $ bracketApp $ \i -> do
  logInfoNS "INIT" "haskell-servant-realworld"
  logInfoNS "INIT" "Listening on port 8080"

  let
    ch  = i ^. #logChannel
    cfg = Config
      { dbPool = i ^. #dbPool
      }

  liftIO
    $ run 8080
    $ application (handle ch cfg)

  where
    handle ch cfg = runToHandler (runChanLoggingT ch) cfg



server
  :: ( MonadUser m
     , MonadThrow m
     )
  => Api (AsServerT m)
server = Api
  { userApi = genericServerT UserApi
    { loginUser, registerUser, getCurrentUser, updateUser }
  }

  where
    loginUser c    = User.loginUser c
    registerUser r = User.registerUser r
    getCurrentUser = User.getUser undefined >>= notFound
    updateUser u   = User.updateUser u


notFound :: MonadThrow m => Maybe a -> m a
notFound =
  maybe (throwM err404) pure
