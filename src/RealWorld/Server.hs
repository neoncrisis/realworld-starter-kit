{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module RealWorld.Server where

import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Control.Monad.Logger (logInfoNS, runChanLoggingT, runStdoutLoggingT, unChanLoggingT)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, MonadLogger)
import Data.Generics.Labels ()
import Database.Persist.Sql (SqlBackend)
import GHC.Generics (Generic)
import Lens.Micro.Platform ((^.))
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import RealWorld.Api (Api (..), EmailAddress (..), Token (..), User (..), UserApi (..),
                      Username (..))
import RealWorld.App (runToHandler)
import RealWorld.Config (Config (..), connectDb)
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
application :: Init -> Application
application i =
    logStdout
  $ genericServeTWithContext (runToHandler (runChanLoggingT ch) cfg) server ctx

  where
    cfg = Config (i ^. #dbPool)
    ch  = i ^. #logChannel
    ctx = EmptyContext


runServer :: MonadUnliftIO m => m ()
runServer = runStdoutLoggingT $ bracketApp $ \i -> do
  logInfoNS "INIT" "haskell-servant-realworld"
  logInfoNS "INIT" "Listening on port 8080"
  liftIO $ run 8080 $ application i


server
  :: Monad m
  => Api (AsServerT m)
server = Api
  { apiUser = genericServerT userApi
  }


userApi
  :: Monad m
  => UserApi (AsServerT m)
userApi = UserApi {..}

  where
    loginUser _ =
      pure User
        { email    = EmailAddress "jake@jake.jake"
        , token    = Token "asdfkj0"
        , username = Username "neoncrisis"
        , bio      = "Hi"
        , image    = Nothing
        }

    registerUser _ =
      pure User
        { email    = EmailAddress "jake@jake.jake"
        , token    = Token "asdfkj0"
        , username = Username "neoncrisis"
        , bio      = "Hi"
        , image    = Nothing
        }

    getCurrentUser =
      pure User
        { email    = EmailAddress "jake@jake.jake"
        , token    = Token "asdfkj0"
        , username = Username "neoncrisis"
        , bio      = "Hi"
        , image    = Nothing
        }

    updateUser _ =
      pure User
        { email    = EmailAddress "jake@jake.jake"
        , token    = Token "asdfkj0"
        , username = Username "neoncrisis"
        , bio      = "Hi"
        , image    = Nothing
        }
