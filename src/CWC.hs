module CWC where

import Control.Concurrent (threadDelay)
import Data.Time
import qualified Data.Time.Horizon as H

-- | State as in state machine
data DoorState = Open | Closed
                deriving (Show, Read)

data WorldState = WorldState 
  { currentTime :: UTCTime
  }
data Event = Even
data PiState = PiState


data Config = Config
  { timeZone :: TimeZone
  , longitute :: H.LongitudeWest
  , latitude :: H.LatitudeNorth
  , sunsetOffset :: Integer
  , sunriseOffset :: Integer
  } deriving (Show, Read)

data GlobalState = GlobalState 
  { config :: Config
  , world :: WorldState
  , pi :: PiState
  }

utcZone = hoursToTimeZone 0
minutesToDiffTime = fromInteger . (60*)

currentDay :: WorldState ->  Day
currentDay = fmap localDay (utcToLocalTime utcZone .currentTime)

utcTimeOfDay :: UTCTime -> TimeOfDay
utcTimeOfDay = localTimeOfDay . utcToLocalTime utcZone
  
  
sunset, sunrise :: GlobalState -> TimeOfDay
sunset = do
  day <- currentDay . world
  long <- longitute .config
  lat <- latitude .config
  offset <- minutesToDiffTime `fmap` ( sunsetOffset . config)
  return $ utcTimeOfDay (addUTCTime offset (H.sunset day long lat))

sunrise = do
  day <- currentDay . world
  long <- longitute . config
  lat <- latitude .config 
  offset <- minutesToDiffTime . (sunriseOffset . config)
  return $ utcTimeOfDay ( addUTCTime offset (H.sunset day long lat))



expectedDoorState :: GlobalState -> DoorState
expectedDoorState = do
  set <- sunset 
  rise <- sunrise 
  currentTime <- utcTimeOfDay `fmap` (currentTime . world)
  return $ if rise <= currentTime && currentTime <= set
             then Open
             else Closed

-- * In

-- * Out
out :: PiState -> IO ()
out _ = return ()

-- | Analyse a world state and generate an event
step :: WorldState -> WorldState -> Maybe Event
step old new = Nothing

transition :: PiState -> Event -> IO PiState
transition pi _ = return pi

-- * Automaton

-- * Misc Functions
-- | Loop the given action every n millisecond.
loop :: Int -> (a -> IO a) -> a -> IO a
loop n f state = do
  threadDelay (1000*n)
  state'  <- f state
  loop n f state'
