{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module RelationsSpec(groupSpec, optimiseSpec) where

import Relations
import Test.Hspec
import qualified Data.HashMap.Strict as Map

type SimpleEvent = PaymentEvent String Int

userA = User { firstName="Alice", lastName="Test" }
userB = User { firstName="Bob", lastName="Test" }
userC = User { firstName="Chris", lastName="Test" }
produceEvent :: SimpleEvent -> SimpleEvent
produceEvent e = e
eventAB = produceEvent Single { eventID=1, who=userA, currency="USD", payee=Map.singleton userB 100 }
eventBC = produceEvent Single { eventID=2, who=userB, currency="USD", payee=Map.singleton userC 150 }
eventCA = produceEvent Single { eventID=3, who=userC, currency="USD", payee=Map.singleton userA 50 }
events = [eventAB, eventBC, eventCA]
expectedResult = Map.fromList [(eventAB, [eventBC]), (eventBC, [eventCA]), (eventCA, [eventAB])]

groupSpec :: Spec
groupSpec = do
  describe "event grouping" $ do
    it "correctly groups events" $
      groupEvents events `shouldBe` expectedResult

optAC = produceEvent Single { eventID=1, who=userA, currency="USD", payee=Map.singleton userC 100 }
optBC = produceEvent Single { eventID=2, who=userB, currency="USD", payee=Map.singleton userC 50 }
optCA = produceEvent Single { eventID=3, who=userC, currency="USD", payee=Map.singleton userA 50 }

optimiseSpec :: Spec
optimiseSpec = do
  describe "one optimisation step" $ do
    it "correctly optimise relations" $
      optimiseStep events `shouldBe` (optAC : optBC : optCA : [])