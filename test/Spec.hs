{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}

import Control.Lens (use, (%=))
import Control.Monad.State.Strict (MonadState, State)
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import RealWorld.Service.User (MonadUser (..))
import RealWorld.Types.User (User)

data St = St
  { users :: Map Int User
  } deriving (Show, Eq, Generic)

main :: IO ()
main = putStrLn "Test suite not yet implemented"


newtype Test a = Test
  { unTest :: State St a }
  deriving (Applicative, Functor, Monad, MonadState St)


instance MonadUser Test where
  loginUser _c = do
    us <- use #users
    case Map.lookup 0 us of
      Nothing -> fail "Failed to login"
      Just u  -> pure u

  registerUser _r = do
    let user = error "Not implemented"
    #users %= Map.insert 0 user
    pure user

  getUser i = do
    us <- use #users
    pure $ Map.lookup i us

  updateUser _u = do
    pure (error "Not implemented")

