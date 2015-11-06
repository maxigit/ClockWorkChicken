module Main where

import CWC

import Data.Time.Clock (getCurrentTime) 
import Data.Time (getCurrentTimeZone)

main :: IO GlobalState
main = do
  utc <- getCurrentTime
  zone <- getCurrentTimeZone

  let state = GlobalState utc zone
                  (20/60) (53.75)
                  0 0 -- offsets
                  Open Open

  loop 1000 mainLoop state where
  mainLoop state = do
    utc <- getCurrentTime
    let newState = state {currentTime = utc }
    print newState
    return newState


    
