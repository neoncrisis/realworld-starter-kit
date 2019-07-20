{-# LANGUAGE DeriveGeneric #-}
module RealWorld.Config where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (SqlBackend)
import GHC.Generics (Generic)


data Config = Config
  { dbPool  :: !(Pool SqlBackend)
  } deriving (Generic)

connectDb
  :: ( MonadLogger m
     , MonadUnliftIO m
     )
  => ByteString
  -> m (Pool SqlBackend)
connectDb c =
  createPostgresqlPool c 100
