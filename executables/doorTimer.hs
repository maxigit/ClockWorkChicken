module Main where

import CWC
import Data.Automaton

import Data.Time.Clock (getCurrentTime) 
import Data.Time (getCurrentTimeZone)

-- automaton :: Automaton 
automaton = Automaton exitOn
                      enterState 
                      exitState
                      transition
                      nextEvent where

  exitOn s = False
  enterState old new = do
    enterState' (piState old) (piState new)
    print new
  enterState' _ Closing = print "close door"
  enterState' _ Opening = print "open door"
  enterState' _ _ = return ()

  exitState old new = return ()
  nextEvent = do
    command <- getChar
    return $ case command of
                'd' -> Just Sunrise -- day
                'n' -> Just Sunset -- night
                'o' -> Just OpenDoor -- open
                'c' -> Just CloseDoor -- close
                'O' -> Just (Door DoorOpened)
                'C' -> Just (Door DoorClosed)
                _ -> Nothing

  transition state ev = return $ case transition' (piState state) ev  of
    Nothing -> state
    Just p ->  state { piState = p }
  transition' Closed Sunrise =  Just Opening
  transition' Closing (Door DoorClosed) =  Just Closed
  transition' Opened Sunset = Just Closing
  transition' Opening (Door DoorOpened) = Just Opened
  transition' _ _ = Nothing
      
      

-- main :: IO GlobalState
main = do
  utc <- getCurrentTime
  zone <- getCurrentTimeZone

  let config = Config zone
                  (20/60) (53.75)
                  0 0 
      global = GlobalState config (WorldState utc) Closed

  runAutomaton automaton global

