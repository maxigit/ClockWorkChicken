module Main where

import CWC
import Data.Automaton

import Data.Time.Clock (getCurrentTime) 
import Data.Time (getCurrentTimeZone)

-- automaton :: Automaton 
automaton = Automaton initial
                      exitOn
                      enterState 
                      exitState
                      transition
                      nextEvent where

  initial = undefined
  exitOn = undefined
  enterState old new = return ()
  exitState old new = return ()
  transition state ev = return state
  nextEvent = return Nothing

-- main :: IO GlobalState
main = do
  utc <- getCurrentTime
  zone <- getCurrentTimeZone

  let config = Config zone
                  (20/60) (53.75)
                  0 0 
      global = GlobalState config (WorldState utc) undefined

  runAutomaton automaton global

