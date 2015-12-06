module CWCSpec where

import CWC
import Control.Monad.State
import Data.Time
import Data.Time.LocalTime
import Test.Hspec
import Test.HUnit
liftIO2  action a = liftIO . action a

spec :: Spec
spec = do
  describe "Sunset/Sunrise" $ do
    context "without time offset" $ do
      let zone = utc
          defConfig = Config zone
                      (48.8567) (2.3508)
                      0 0
          defWorld = WorldState $ UTCTime (fromGregorian 2015 12 03) 0
          io = error "io not defined @toto"
          defGlobal :: GlobalState IO
          defGlobal = GlobalState defConfig defWorld io
      

      context "Paris 03 Dec 2015" $ do
        let parisSunrise = TimeOfDay 08 25 00
            parisSunset = TimeOfDay 16 56 00
            global = defGlobal { world = defWorld { currentTime =
              UTCTime (fromGregorian 2015 12 03) 0 }}
            shouldBe' = liftIO2 shouldBe

        it "should have correct sunrise" $ flip evalStateT global $ do
          rise <- sunrise
          rise `shouldBe'` parisSunrise

        it "should have correct sunset" $ flip evalStateT global $ do
          set <- sunset
          set `shouldBe'` parisSunset

      context "Paris 03 Jun 2016" $ do
        let parisSunrise = TimeOfDay 05 51 00
            parisSunset = TimeOfDay 21 47 00
            global = defGlobal { world = defWorld { currentTime =
              UTCTime (fromGregorian 2015 06 03) 0 }}
            shouldBe' = liftIO2 shouldBe

        it "should have correct sunrise" $ flip evalStateT global $ do
          rise <- sunrise
          rise `shouldBe'` parisSunrise

        it "should have correct sunset" $ flip evalStateT global $ do
          set <- sunset
          set `shouldBe'` parisSunset

  describe "event generation" $ do
    context "when sunrise" $ do
      it "should send a SUNRISE event"
        pending
    context "when sunset" $ do
      it "should send a SUNSET event"
        pending

    context "when sun already rised" $ do
      it "shouldn't sent any event" $ do
        pending
    context "when sun already sunset" $ do
      it "shouldn't sent any event" $ do
        pending

    context "door closed" $ do
      it "should send a DoorClosed event" $ do
        pending

    context "door opened" $ do
      it "should send a DoorOpened event" $ do
        pending

    context "in Sunset display mode" $ do
      it "should come back in normal mode after 5s" $ do
        pending

  describe "event processing:" $ do
    context "Sunrise" $ do
      it "should open the door" $ do
        pending
    context "Sunset" $ do
      it "should close the door" pending

    context "Press the open door button" $ do
      it "should open the door" $ do
        pending

    context "Press the close door button" $ do
      it "should close the door" $ do
        pending

    context "Door closed" $ do
      it "should lock the door" $ do
        pending

    context "Lock opened" $ do
      it "should open the door" $ do
        pending
    context "when the next display button is pressed" $ do
      it "" pending
      


  describe "display" $ do
    it "should display current time" pending
    context "in sunset mode" $ do
      it "should display sunset time" pending
    context "in sunrise mode" $ do
      it "should display sunrise time" pending

    

