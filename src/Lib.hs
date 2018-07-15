{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , stateApp
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

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

data Users = Users
  { users :: MVar [User]
  }

type AppM = ReaderT Users Handler

$(deriveJSON defaultOptions ''User)

type API = "users" :> ( Get '[JSON] [User] :<|> (ReqBody '[JSON] User :> Post '[JSON] User))

startApp :: IO ()
startApp = startStateApp defaultUsers

startStateApp :: [User] -> IO ()
startStateApp users = do
  v <- newMVar users
  let userVar = Users v
  run 8080 $ stateApp userVar

api :: Proxy API
api = Proxy

insertUser :: Users -> User -> IO ()
insertUser (Users v) user = do
  users <- takeMVar v
  putMVar v (user : users)

lookupUsers :: Users -> IO [User]
lookupUsers (Users v) = do
  users <- takeMVar v
  putMVar v users
  return users


stateServer :: ServerT API AppM
stateServer = getUsers :<|> createUser
    where getUsers :: AppM [User]
          getUsers = do
            users <- ask
            liftIO $ lookupUsers users

          createUser :: User -> AppM User
          createUser user = do
            users <- ask
            liftIO $ insertUser users user
            return user

nt :: Users -> AppM a -> Handler a
nt s x = runReaderT x s

stateApp :: Users -> Application
stateApp users = serve api $ hoistServer api (nt users) stateServer

defaultUsers :: [User]
defaultUsers = [ User 1 "Isaac" "Newton"
               , User 2 "Albert" "Einstein"
               ]

