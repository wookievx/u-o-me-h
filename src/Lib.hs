{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , stateApp
    , defaultUsers
    , Users(Users)
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Concurrent
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class      (liftIO)
import Servant
import Relations

data ExampleUser = ExampleUser
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

data Users = Users
  { users :: MVar [ExampleUser]
  }

type AppM = ReaderT Users Handler

$(deriveJSON defaultOptions ''ExampleUser)

type API = "users" :> ( Get '[JSON] [ExampleUser] :<|> (ReqBody '[JSON] ExampleUser :> Post '[JSON] ExampleUser))

startApp :: IO ()
startApp = startStateApp defaultUsers

startStateApp :: [ExampleUser] -> IO ()
startStateApp users = do
  v <- newMVar users
  let userVar = Users v
  run 8080 $ stateApp userVar

api :: Proxy API
api = Proxy

insertUser :: Users -> ExampleUser -> IO ()
insertUser (Users v) user = do
  users <- takeMVar v
  putMVar v (user : users)

lookupUsers :: Users -> IO [ExampleUser]
lookupUsers (Users v) = do
  users <- takeMVar v
  putMVar v users
  return users


stateServer :: ServerT API AppM
stateServer = getUsers :<|> createUser
    where getUsers :: AppM [ExampleUser]
          getUsers = do
            users <- ask
            liftIO $ lookupUsers users

          createUser :: ExampleUser -> AppM ExampleUser
          createUser user = do
            users <- ask
            liftIO $ insertUser users user
            return user

nt :: Users -> AppM a -> Handler a
nt s x = runReaderT x s

stateApp :: Users -> Application
stateApp users = serve api $ hoistServer api (nt users) stateServer

defaultUsers :: [ExampleUser]
defaultUsers = [ ExampleUser 1 "Isaac" "Newton"
               , ExampleUser 2 "Albert" "Einstein"
               ]

