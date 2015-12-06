module CWCHelper where

import CWC
import System.RaspberryPi

import Data.Time

import Control.Monad.State

-- | Set the current time of a GlobalState
setCurrentTime :: (Integer, Int, Int) -> (Integer, Integer, Integer) -> GlobalState m -> GlobalState m

setCurrentTime (year, month, day) (hour, minute, second) state =
  let d = fromGregorian year month day
      t = second + 60 * (minute + 60* hour)

      world = CWC.world state

  in state { world =  world { currentTime = UTCTime d (secondsToDiffTime t) } }
  

-- | mock readWorld to set the next current time 
setNextCurrentTime :: Monad m => (Integer, Int, Int) -> (Integer, Integer, Integer) -> GlobalState m -> GlobalState m

setNextCurrentTime day time state = let
  read = do
    st <- get
    return $ world (setCurrentTime day time st)
  in modifyIoEx (\i -> i { readWorld = read }) state

modifyIoEx :: (PiIO m -> PiIO m) ->  GlobalState m -> GlobalState m
modifyIoEx f state = let
  io' = io state
  ex = extension io'
  in state { io = io' { extension = f ex } }



