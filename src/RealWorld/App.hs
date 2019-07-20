{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
module RealWorld.App where

import Control.Exception.Safe (try)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO (..), UnliftIO (..), withUnliftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), runReaderT)
import RealWorld.Config (Config)
import Servant.Server (Handler (..))


type App = AppT IO

newtype AppT m a = AppT
  { unAppT :: ReaderT Config m a
  } deriving (Applicative, Functor, Monad)

deriving instance (MonadIO m) => MonadIO (AppT m)
deriving instance (Monad m) => MonadReader Config (AppT m)

-- MonadUnliftIO is a *much* safer implementation along the lines of MonadBaseControl
-- Primarily it has no chance of losing monad state when performing concurrent operations
instance MonadUnliftIO m => MonadUnliftIO (AppT m) where
  askUnliftIO =
    AppT $
    ReaderT $ \r ->
    withUnliftIO $ \u ->
      pure (UnliftIO (unliftIO u . flip runReaderT r . unAppT))


runAppT
  :: Config
  -> AppT m a
  -> m a
runAppT cfg app =
  runReaderT (unAppT app) cfg

-- Wrap our monad in ExceptT to conform to servant's expectations
-- We do not run in ExceptT because we want only one way to throw/catch exceptions (IO)
-- @see https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html
runToHandler
  :: Config
  -> App a
  -> Handler a
runToHandler cfg =
  Handler . ExceptT . try . runAppT cfg
