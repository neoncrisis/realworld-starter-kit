{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RealWorld.Types.User where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

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
