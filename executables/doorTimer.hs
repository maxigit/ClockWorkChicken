module Main where

import CWC

import Data.Time.Clock (getCurrentTime) 
import Data.Time (getCurrentTimeZone)

main :: IO GlobalState
main = do
  utc <- getCurrentTime
  zone <- getCurrentTimeZone

  let state = GlobalState utc zone Open Open)

  loop 1000 mainLoop state where
  mainLoop state = do
    utc <- getCurrentTime
    let newState = setTime utc state
    print newState
    return newState


    
