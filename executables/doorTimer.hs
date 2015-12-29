module Main where

import CWC
import Data.Automaton

import Data.Char (toLower)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime) 
import Data.Time (getCurrentTimeZone)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class

import System.RaspberryPi

testEx ::  PiIO IO 
testEx = PiIO readWorld
              displayWorld
              openDoor
              closeDoor
              lockDoor
              unlockDoor
              displayTime
  where
    readWorld :: GState IO WorldState
    readWorld    = do
      oldWorld <- gets world
      liftIO $ print "do you want to increase time?"
      answer <- liftIO getLine
      if (map toLower answer) `elem` ["y", "yes"]
         then let time = currentTime oldWorld
                  newTime = addUTCTime (3600) time 
              in return oldWorld {currentTime = newTime }
              

         else return oldWorld
         -- | Display all mode with a * in front of the current mode
    displayWorld world mode = do
      let modes = [minBound..maxBound]
      displays <- mapM displayFromState modes
      let format s m | m == mode = "*** " ++ s ++ " ***"
                     | otherwise = "    " ++ s ++ "    "
      liftIO $ mapM_ putStrLn (zipWith format displays modes)


    openDoor     = error "todo :openDoor"
    closeDoor    = error "todo :closeDoor"
    lockDoor     = error "todo :lockDoor"
    unlockDoor   = error "todo :unlockDoor"
    displayTime  = error "todo :displayTime"

testPi :: Pi IO (PiIO IO) 
testPi  = Pi  
    readPin
    writePin
    testEx
    (error "run not defined")
  where
    readPin pin = do
      putStrLn ("read pin #" ++ show pin)
      fmap read getLine
    readTime = do
      getCurrentTime
    writePin pin value = do
      putStrLn ("write pin #" ++ show pin ++ " -> " ++ show value)

-- main :: IO GlobalState
main = do
  utc <- getCurrentTime
  zone <- getCurrentTimeZone

  let config = Config zone
                  (20/60) (53.75)
                  0 0 
      world = WorldState utc
      io = testPi
      global = GlobalState config world io

      initialPi = PiState DoorClosed DoorClosed TimeM
  evalStateT (runAutomaton automaton initialPi) global

