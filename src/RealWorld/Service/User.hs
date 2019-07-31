{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
module RealWorld.Service.User where

import RealWorld.Types.User

class Monad m => MonadUser m where
  loginUser    :: Credentials -> m User
  registerUser :: RegisterUser -> m User
  getUser      :: Int -> m (Maybe User)
  updateUser   :: UpdateUser -> m User


user :: User
user = User
  { email = EmailAddress ""
  , token = Token ""
  , username = Username ""
  , bio = ""
  , image = Nothing
  }
