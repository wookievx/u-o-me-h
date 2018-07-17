{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Relations
  ( registerPayment
  , groupEvents
  , optimiseStep
  , User(User)
  , firstName
  , lastName
  , Money(Money)
  , mCur
  , amount
  , PaymentEvent(Single)
  , eventID
  , who
  , currency
  , payee
  ) where

import qualified Data.HashMap.Strict as Map
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Data.Hashable
import Control.Concurrent
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)

data User = User { firstName :: String, lastName :: String } deriving (Eq, Show, Generic)
instance Hashable User

data Money c a = Money { mCur :: c, amount :: a} deriving (Eq, Show, Generic)
instance (Hashable c, Hashable a) => Hashable (Money c a)

data PaymentEvent c a = Single
  { eventID :: Int
  , who :: User
  , currency :: c
  , payee :: Map.HashMap User a
  } deriving Show

instance Eq (PaymentEvent c a) where
  (==) l r = eventID l == eventID r

instance Hashable (PaymentEvent c a) where
  hashWithSalt salt event = hashWithSalt salt $ eventID event

instance Ord (PaymentEvent c a) where
  (Single{eventID=l}) <= (Single{eventID=r}) = l <= r

type EventLog c a = MVar [PaymentEvent c a]

registerPayment :: Num a => EventLog c a -> PaymentEvent c a -> IO (Money c a)
registerPayment v (single @ Single {payee=p}) = do
  events <- takeMVar v
  putMVar v (single : events)
  return $ calculate single
  where calculate :: Num a => PaymentEvent c a -> Money c a
        calculate Single {currency=cur, payee=p} = Money { mCur=cur, amount=(foldl (+) 0 p) }

type GroupedEvents c a = Map.HashMap (PaymentEvent c a) [PaymentEvent c a]

groupEvents :: (Hashable a, Eq a, Hashable c, Eq c) => [PaymentEvent c a] -> GroupedEvents c a
groupEvents events = Map.fromList . (map unpack) $ events where
  unpack event @ Single { payee=p } = (event, filter fromPayee events) where
    payeeUsers = (map fst . Map.toList . payee) $ event
    fromPayee Single { who=w } = elem w payeeUsers

optimiseStep :: (Hashable a, Eq a, Hashable c, Eq c) => [PaymentEvent c a] -> [PaymentEvent c a]
optimiseStep events = Set.elems . foldl step Set.empty . sort . Map.toList $ grouped where
            grouped = groupEvents events
            step s ((f, [])) = Set.insert f s
            step s ((f, elems)) = undefined