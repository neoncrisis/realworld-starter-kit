{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module RealWorld.Api where

import GHC.Generics (Generic)
import RealWorld.Types.User
import Servant.API
import Servant.API.Generic


data Api route = Api
  { userApi :: !(ToServant UserApi route)
  } deriving (Generic)


data UserApi route = UserApi
  { loginUser :: route
    :- "api" :> "users" :> "login"
    :> ReqBody '[JSON] Credentials
    :> Post '[JSON] User

  , registerUser :: route
    :- "api" :> "users"
    :> ReqBody '[JSON] RegisterUser
    :> Post '[JSON] User

  , getCurrentUser :: route
    :- "api" :> "user"
    :> Get '[JSON] User

  , updateUser :: route
    :- "api" :> "user"
    :> ReqBody '[JSON] UpdateUser
    :> Put '[JSON] User

  } deriving (Generic)
