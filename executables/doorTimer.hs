module Main where

import CWC

import Data.Time.Clock (getCurrentTime)
import Data.Time

main :: IO ()
main = loop 1000 mainLoop where
  mainLoop = do
    utc <- getCurrentTime
    print utc
    
