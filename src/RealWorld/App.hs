{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RankNTypes                 #-}
module RealWorld.App where

import Control.Exception.Safe (MonadCatch, MonadThrow, try)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO (..), UnliftIO (..), withUnliftIO)
import Control.Monad.Logger (MonadLogger (..))
import Control.Monad.Reader (MonadReader, ReaderT (..), runReaderT)
import Data.Generics.Labels ()
import RealWorld.Config (Config)
import RealWorld.Service.User as User (MonadUser (..), user)
import Servant.Server (Handler (..))


type App = AppT IO

newtype AppT m a = AppT
  { unAppT :: ReaderT Config m a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadLogger
    , MonadReader Config
    , MonadThrow
    , MonadCatch
    )

-- MonadUnliftIO is a *much* safer implementation along the lines of MonadBaseControl
-- Primarily it has no chance of losing monad state when performing concurrent operations
instance MonadUnliftIO m => MonadUnliftIO (AppT m) where
  askUnliftIO =
    AppT $
    ReaderT $ \r ->
    withUnliftIO $ \u ->
      pure $ UnliftIO (unliftIO u . flip runReaderT r . unAppT)

instance Monad m => MonadUser (AppT m) where
  loginUser _    = pure user
  registerUser _ = pure user
  getUser _      = pure (Just user)
  updateUser _   = pure user



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
  :: (forall b. m b -> IO b)
  -> Config
  -> AppT m a
  -> Handler a
runToHandler runToIO cfg app =
  Handler $ ExceptT $ try $ runToIO $ runAppT cfg app
