module Main where

import CWC
import Data.Automaton

import Data.Time.Clock (getCurrentTime, UTCTime) 
import Data.Time (getCurrentTimeZone)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class

data RPI mi mo = RPI 
  { readPin :: Int -> mi Double
  , readTime :: mi UTCTime
  , writePin :: Int -> Double -> mo ()
  }

testPI  = RPI 
  readPin
  readTime
  writePin where
    readPin pin = do
      putStrLn ("read pin #" ++ show pin)
      fmap read getLine
    readTime = do
      getCurrentTime
    writePin pin value = do
      putStrLn ("write pin #" ++ show pin ++ " -> " ++ show value)

automaton :: Automaton (StateT WorldState IO) PiState Event
automaton = Automaton exitOn
                      enterState 
                      exitState
                      transition
                      nextEvent where

  exitOn s = False
  enterState old new = do
    enterState' old new
    liftIO (print new)
  enterState' _ Closing = liftIO (print "close door")
  enterState' _ Opening = liftIO (print "open door" )
  enterState' _ _ = return ()

-- How to update currentTime in an automaton context
-- use state monad with world state ?
-- so that nextEvent can access and modify it ?
--
-- we need also a way to "tick", ie modify the state
-- even though nothing happend
  exitState old new = return ()
  nextEvent = do
    command <- liftIO getChar
    return $ case command of
                'd' -> Just Sunrise -- day
                'n' -> Just Sunset -- night
                'o' -> Just OpenDoor -- open
                'c' -> Just CloseDoor -- close
                'O' -> Just (Door DoorOpened)
                'C' -> Just (Door DoorClosed)
                _ -> Nothing

  transition Closed Sunrise =  Opening
  transition Closing (Door DoorClosed) =  Closed
  transition Opened Sunset = Closing
  transition Opening (Door DoorOpened) = Opened
  transition state _ = state
      
      

-- main :: IO GlobalState
main = do
  utc <- getCurrentTime
  zone <- getCurrentTimeZone

  let config = Config zone
                  (20/60) (53.75)
                  0 0 
      world = WorldState utc
      global = GlobalState config world Closed

  evalStateT (runAutomaton automaton Closed) world

