module CWCSpec where

import CWC
import CWCHelper
import Data.Automaton
import System.RaspberryPi

-- import System.RaspberryPi.Mock.IO

import Control.Monad.State
import Data.Time
import Data.Time.LocalTime

import Test.Hspec hiding(shouldBe)
import Test.Hspec.Expectations.Lifted(shouldBe)
-- import Test.HUnit

spec :: Spec
spec = do
  let zone = utc
      defConfig = Config zone
                  (-2.3508) (48.8567) 
                  0 0
      defWorld = WorldState $ UTCTime (fromGregorian 2015 12 03) 0
      defExtension :: PiIO IO
      defExtension = piIO
      defIo = Pi (error "readPin")
                 (error "writePin")
                 defExtension
                (error "run")
      defGlobal :: GlobalState IO
      defGlobal = GlobalState defConfig defWorld defIo
      
  describe "Sunset/Sunrise" $ do
    context "without time offsets" $ do
      context "Paris 03 Dec 2015" $ do
        let parisSunrise = TimeOfDay (08-1) 26 11
            parisSunset = TimeOfDay (16-1) 57 18
            global = defGlobal { world = defWorld { currentTime =
              UTCTime (fromGregorian 2015 12 03) 0 }}

        it "should have correct sunrise" $ flip evalStateT global $ do
          rise <- liftG sunrise
          rise `shouldBe` parisSunrise

        it "should have correct sunset" $ flip evalStateT global $ do
          set <- liftG sunset
          set `shouldBe` parisSunset

      context "Paris 03 Jun 2016" $ do
        let parisSunrise = TimeOfDay (05-2) 52 02
            parisSunset = TimeOfDay (21-2) 48 16
            global = defGlobal { world = defWorld { currentTime =
              UTCTime (fromGregorian 2015 06 03) 0 }}

        it "should have correct sunrise" $ flip evalStateT global $ do
          rise <- liftG sunrise
          rise `shouldBe` parisSunrise

        it "should have correct sunset" $ flip evalStateT global $ do
          set <- liftG sunset
          set `shouldBe` parisSunset

    context "with time offset" $ do
      let config = defConfig { sunriseOffset = 30 
                             , sunsetOffset = -30
                             }
      context "Paris 03 Dec 2015" $ do
        let parisSunrise = TimeOfDay (08-1) 56 11
            parisSunset = TimeOfDay (16-1) 27 18
            global = defGlobal { world = defWorld { currentTime =
              UTCTime (fromGregorian 2015 12 03) 0 }
              , config = config
              }

        it "should have correct sunrise" $ flip evalStateT global $ do
          rise <- liftG sunrise
          rise `shouldBe` parisSunrise

        it "should have correct sunset" $ flip evalStateT global $ do
          set <- liftG sunset
          set `shouldBe` parisSunset

      context "Paris 03 Jun 2016" $ do
        let parisSunrise = TimeOfDay (05-2+1) 22 02
            parisSunset = TimeOfDay (21-2) 18 16
            global = defGlobal { world = defWorld { currentTime =
              UTCTime (fromGregorian 2015 06 03) 0 }
              , config = config
              }

        it "should have correct sunrise" $ flip evalStateT global $ do
          rise <- liftG sunrise
          rise `shouldBe` parisSunrise

        it "should have correct sunset" $ flip evalStateT global $ do
          set <- liftG sunset
          set `shouldBe` parisSunset

  describe "event generation" $ do
    context "when sunrise" $ do
      -- stub readlWorld to change the current time to day
      let readWorld' :: GState IO WorldState
          readWorld' = do
            world <- gets world
            let time = currentTime world
            let newTime = time { utctDayTime = 12*3600 }
            return world { currentTime = newTime }

          global :: GlobalState IO 
          global = defGlobal { io
                             = defIo { extension
                                     = (extension defIo)
                                    { readWorld = readWorld' } }
                              }
                                              
      it "should send a SUNRISE event" $ flip evalStateT global $ do
        ev <- nextEvent automaton
        ev `shouldBe` (Just Sunrise)

    context "when sunset" $ do
      let global = setNextCurrentTime (2015, 12, 3) (23, 30, 0)
              (setCurrentTime (2015, 12, 3) (12,0,0) defGlobal)
      it "should send a SUNSET event" $ flip evalStateT global $ do
        ev <- nextEvent automaton
        ev `shouldBe` (Just Sunset)

    context "when sun already rised" $ do
      let global = setNextCurrentTime (2015, 12, 3) (14, 30, 0)
              (setCurrentTime (2015, 12, 3) (12,0,0) defGlobal)
      it "shouldn't sent any event" $ flip evalStateT global $ do
        ev <- nextEvent automaton
        ev `shouldBe` Nothing

    context "when sun already set" $ do
      let global = setNextCurrentTime (2015, 12, 3) (02, 00, 0)
              (setCurrentTime (2015, 12, 3) (02,30,0) defGlobal)
      it "shouldn't sent any event" $ flip evalStateT global $ do
        ev <- nextEvent automaton
        ev `shouldBe` Nothing

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

    

