{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module RelationsSpec(groupSpec) where

import Relations
import Test.Hspec
import qualified Data.HashMap.Strict as Map

type SimpleEvent = PaymentEvent String Int

userA = User { firstName="Alice", lastName="Test" }
userB = User { firstName="Bob", lastName="Test" }
userC = User { firstName="Chris", lastName="Test" }
produceEvent :: SimpleEvent -> SimpleEvent
produceEvent e = e
eventAB = produceEvent Single { who=userA, currency="USD", payee=Map.singleton userB 100 }
eventBC = produceEvent Single { who=userB, currency="USD", payee=Map.singleton userC 150 }
eventCA = produceEvent Single { who=userC, currency="USD", payee=Map.singleton userA 50 }
events = [eventAB, eventBC, eventCA]
expectedResult = Map.fromList [(eventAB, [eventBC]), (eventBC, [eventCA]), (eventCA, [eventAB])]

groupSpec :: Spec
groupSpec = do
  describe "event grouping" $ do
    it "correctly groups events" $
      groupEvents events `shouldBe` expectedResult
