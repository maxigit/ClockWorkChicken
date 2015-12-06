module CWCSpec where

import CWC
import Test.Hspec
import Test.HUnit

spec :: Spec
spec = do
  describe "Sunset/Sunrise" $ do
    context "Paris 03 Dec 2015" $ do
      it "should have correct sunrise" $ do
        pending
      it "should have correct sunset" $ do
        pending
    context "Paris 03 Jun 2016" $ do
      it "should have correct sunrise" $ do
        pending
      it "should have correct sunset" $ do
        pending

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

    

