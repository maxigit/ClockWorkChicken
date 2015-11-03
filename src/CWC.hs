module CWC where

import Control.Concurrent (threadDelay)
import Data.Time

-- | State as in state machine
data DoorState = Open | Closed
                deriving (Show, Read)


data GlobalState = GlobalState
  { localTime :: LocalTime
  , timeZone :: TimeZone
  , doorState :: DoorState
  , doorDesiredState :: DoorState
  } deriving (Show, Read)

  

setTime :: UTCTime -> GlobalState -> GlobalState
setTime utc state = let
  localTime = utcToLocalTime (timeZone state) utc
  in state { localTime = localTime }

-- * Misc Functions
-- | Loop the given action every n millisecond.
loop :: Int -> (a -> IO a) -> a -> IO a
loop n f state = do
  threadDelay (1000*n)
  state'  <- f state
  loop n f state'
