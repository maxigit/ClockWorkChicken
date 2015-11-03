module CWC where

import Control.Concurrent (threadDelay)
import Data.Time
import qualified Data.Time.Horizon as H

-- | State as in state machine
data DoorState = Open | Closed
                deriving (Show, Read)


data GlobalState = GlobalState
  { currentTime :: UTCTime
  , timeZone :: TimeZone
  , longitute :: H.LongitudeWest
  , latitude :: H.LatitudeNorth
  , sunsetOffset :: Integer
  , sunriseOffset :: Integer
  , doorState :: DoorState
  , doorDesiredState :: DoorState
  } deriving (Show, Read)


utcZone = hoursToTimeZone 0
minutesToDiffTime = fromInteger . (60*)

currentDay :: GlobalState ->  Day
currentDay = fmap localDay (utcToLocalTime utcZone .currentTime)

utcTimeOfDay :: UTCTime -> TimeOfDay
utcTimeOfDay = localTimeOfDay . utcToLocalTime utcZone
  
  
sunset, sunrise :: GlobalState -> TimeOfDay
sunset = do
  day <- currentDay
  long <- longitute
  lat <- latitude
  offset <- minutesToDiffTime `fmap` sunsetOffset
  return $ utcTimeOfDay (addUTCTime offset (H.sunset day long lat))

sunrise = do
  day <- currentDay
  long <- longitute
  lat <- latitude
  offset <- minutesToDiffTime . sunriseOffset
  return $ utcTimeOfDay ( addUTCTime offset (H.sunset day long lat))



expectedDoorState :: GlobalState -> DoorState
expectedDoorState = do
  set <- sunset 
  rise <- sunrise 
  currentTime <- utcTimeOfDay `fmap` currentTime
  return $ if rise <= currentTime && currentTime <= set
             then Open
             else Closed


-- * Misc Functions
-- | Loop the given action every n millisecond.
loop :: Int -> (a -> IO a) -> a -> IO a
loop n f state = do
  threadDelay (1000*n)
  state'  <- f state
  loop n f state'
