{-# LANGUAGE TemplateHaskell #-}
module Relations (registerPayment) where

import qualified Data.Map.Strict as Map
import Control.Concurrent
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)

data User = User String String deriving (Eq, Show)
data Money c a = Money { mCur :: c, amount :: a} deriving (Eq, Show)

data PaymentEvent c a = Single
  { who :: User
  , currency :: c
  , payee :: Map.Map User a
  } deriving (Eq, Show)

type EventLog c a = MVar [PaymentEvent c a]

registerPayment :: Num a => EventLog c a -> PaymentEvent c a -> IO (Money c a)
registerPayment v (single @ Single {payee=p}) = do
  events <- takeMVar v
  putMVar v (single : events)
  return $ calculate single
  where calculate :: Num a => PaymentEvent c a -> Money c a
        calculate Single {currency=cur, payee=p} = Money { mCur=cur, amount=(foldl (+) 0 p) }
