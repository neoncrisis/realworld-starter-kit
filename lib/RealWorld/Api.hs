{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
module RealWorld.Api where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.API.Generic


data Api route = Api
  { apiUser :: !(ToServant UserApi route)
  } deriving (Generic)


data Credentials = Credentials
  { email    :: !EmailAddress
  , password :: !Password
  } deriving (Eq, Show, Generic)

instance ToJSON Credentials
instance FromJSON Credentials


data RegisterUser = RegisterUser
  { email    :: !EmailAddress
  , password :: !Password
  , username :: !Username
  } deriving (Eq, Show, Generic)

instance ToJSON RegisterUser
instance FromJSON RegisterUser


newtype EmailAddress = EmailAddress Text
  deriving (Eq, Show, ToJSON, FromJSON)

newtype Token = Token Text
  deriving (Eq, Show, ToJSON, FromJSON)

newtype Username = Username Text
  deriving (Eq, Show, ToJSON, FromJSON)

newtype Password = Password Text
  deriving (Eq, Show, ToJSON, FromJSON)

newtype Image = Image Text
  deriving (Eq, Show, ToJSON, FromJSON)


-- Comment
-- { "id": 1
-- , "createdAt": "2016-02-18T03:22:56.637Z"
-- , "updatedAt": "2016-02-18T03:22:56.637Z"
-- , "body": "It takes a Jacobian"
-- , "author":
--   { "username": "jake"
--   , "bio": "I work at statefarm"
--   , "image": "https://i.stack.imgur.com/xHWG8.jpg"
--   , "following": false
--   }
-- }

-- Article
-- { "slug": "how-to-train-your-dragon"
-- , "title": "How to train your dragon"
-- , "description": "Ever wonder how?"
-- , "body": "It takes a Jacobian"
-- , "tagList": ["dragons", "training"]
-- , "createdAt": "2016-02-18T03:22:56.637Z"
-- , "updatedAt": "2016-02-18T03:48:35.824Z"
-- , "favorited": false
-- , "favoritesCount": 0
-- , "author":
--   { "username": "jake"
--   , "bio": "I work at statefarm"
--   , "image": "https://i.stack.imgur.com/xHWG8.jpg"
--   , "following": false
--   }
-- }


data Profile = Profile
  { username  :: !Username
  , bio       :: !Text
  , image     :: !(Maybe Image)
  , following :: !Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Profile
instance FromJSON Profile


data User = User
  { email    :: !EmailAddress
  , token    :: !Token
  , username :: !Username
  , bio      :: !Text
  , image    :: !(Maybe Image)
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User


data UpdateUser = UpdateUser
  { email    :: !(Maybe EmailAddress)
  , username :: !(Maybe Username)
  , password :: !(Maybe Password)
  , bio      :: !(Maybe Text)
  , image    :: !(Maybe Image)
  } deriving (Eq, Show, Generic)

instance ToJSON UpdateUser
instance FromJSON UpdateUser



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
