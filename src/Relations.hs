{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Relations
  ( registerPayment
  , groupEvents
  , User(User)
  , firstName
  , lastName
  , Money(Money)
  , mCur
  , amount
  , PaymentEvent(Single)
  , who
  , currency
  , payee
  ) where

import qualified Data.HashMap.Strict as Map
import GHC.Generics (Generic)
import Data.Hashable
import Control.Concurrent
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)

data User = User { firstName :: String, lastName :: String } deriving (Eq, Show, Generic)
instance Hashable User

data Money c a = Money { mCur :: c, amount :: a} deriving (Eq, Show, Generic)
instance (Hashable c, Hashable a) => Hashable (Money c a)

data PaymentEvent c a = Single
  { who :: User
  , currency :: c
  , payee :: Map.HashMap User a
  } deriving (Eq, Show, Generic)

instance (Hashable c, Hashable a) => Hashable (PaymentEvent c a)

type EventLog c a = MVar [PaymentEvent c a]

registerPayment :: Num a => EventLog c a -> PaymentEvent c a -> IO (Money c a)
registerPayment v (single @ Single {payee=p}) = do
  events <- takeMVar v
  putMVar v (single : events)
  return $ calculate single
  where calculate :: Num a => PaymentEvent c a -> Money c a
        calculate Single {currency=cur, payee=p} = Money { mCur=cur, amount=(foldl (+) 0 p) }


groupEvents :: (Hashable a, Eq a, Hashable c, Eq c) => [PaymentEvent c a] -> Map.HashMap (PaymentEvent c a) [PaymentEvent c a]
groupEvents events = Map.fromList . (map unpack) $ events where
  unpack event @ Single { payee=p } = (event, filter fromPayee events) where
    payeeUsers = (map fst . Map.toList . payee) $ event
    fromPayee Single { who=w } = elem w payeeUsers