{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}

import Control.Exception.Safe (MonadCatch, MonadThrow, bracket, try)
import Control.Lens (use, (%=))
import Control.Monad.Except (ExceptT (..))
import Control.Monad.State.Strict (MonadState, StateT, evalStateT)
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (openFreePort, run)
import RealWorld.Server (application)
import RealWorld.Service.User (MonadUser (..))
import RealWorld.Types.User (User)
import Servant.Client (BaseUrl (..), ClientM, Scheme (Http), ServantError, mkClientEnv, runClientM)
import Servant.Server (Handler (..))
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO.Concurrent (ThreadId, forkIO, killThread)


type Step =
  String -> IO ()

main :: IO ()
main =
  defaultMain tests

tests :: TestTree
tests =
  testGroup "RealWorld"
    [ testGroup "User"
      [ testCaseSteps "login" testLoginUser
      ]
    ]

testLoginUser :: Step -> Assertion
testLoginUser step = withServer initialSt $ \srv -> do
  step (show srv)
  fail "not implemented"



-- * State

data St = St
  { users :: Map Int User
  } deriving (Show, Eq, Generic)

initialSt :: St
initialSt = St
  { users = mempty
  }


-- * Test Monad

newtype Test a = Test
  { unTest :: StateT St IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadState St
    , MonadThrow
    , MonadCatch
    )

runTest :: St -> Test a -> Handler a
runTest st a =
  Handler $ ExceptT $ try $ evalStateT (unTest a) st


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


-- * Server

data Server = Server
  { threadId :: ThreadId
  , port     :: Int
  } deriving (Eq, Show, Generic)

withServer :: St -> (Server -> IO a) -> IO a
withServer st =
  bracket (runServer st) (killThread . threadId)

runServer :: St -> IO Server
runServer st = do
  port <- fst <$> openFreePort
  tid  <- forkIO
    $ run port
    $ application
    $ runTest st
  pure $ Server tid port

runClient :: Server -> ClientM a -> IO (Either ServantError a)
runClient Server{port} c = do
  mgr <- newManager defaultManagerSettings
  let
    url = BaseUrl Http "localhost" port ""
    env = mkClientEnv mgr url
  runClientM c env

