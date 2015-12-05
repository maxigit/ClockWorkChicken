module CWC where

import Control.Concurrent (threadDelay)
import Data.Time
import qualified Data.Time.Horizon as H
import Data.Automaton


-- * State
-- | The state (actual or desired) of a door
data DoorState = DoorOpened | DoorClosed
                deriving (Show, Read, Eq)

-- | Input coming from the outside world
-- hold current time, and the state of different sensors
data WorldState = WorldState 
  { currentTime :: UTCTime
  } deriving (Show, Read)

-- | What to display on the screen/LCD
data DisplayMode = TimeM
                 | SunriseM
                 | SunsetM
                 | LongitudeM
                 | LatitudeM
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Event to react on
data Event = Sunrise
           | Sunset 
           | OpenDoor
           | CloseDoor
           | Door DoorState
           | Display DisplayMode
     deriving (Show, Eq)

-- | Abstract state of the PI
-- will be converted in GPIO action
data PiState = Closed
             | Opening
             | Opened
             | Closing

     deriving (Show, Read, Eq)


-- | Miscellaneous configuration
data Config = Config
  { timeZone :: TimeZone
  , longitute :: H.LongitudeWest
  , latitude :: H.LatitudeNorth
  , sunsetOffset :: Integer
  , sunriseOffset :: Integer
  } deriving (Show, Read)

-- | State combining everything
data GlobalState = GlobalState 
  { config :: Config
  , world :: WorldState
  , piState :: PiState
  } deriving (Show, Read)

instance Similar PiState where
  a === b = a == b

instance Similar GlobalState where
  a === b = piState a === piState b

-- * Time related functions

utcZone = hoursToTimeZone 0
minutesToDiffTime = fromInteger . (60*)

currentDay :: WorldState ->  Day
currentDay = fmap localDay (utcToLocalTime utcZone .currentTime)

utcTimeOfDay :: UTCTime -> TimeOfDay
utcTimeOfDay = localTimeOfDay . utcToLocalTime utcZone
  
  
sunset, sunrise :: GlobalState -> TimeOfDay
-- Compute the sunset time corresponding to the GlobalState
sunset = do
  day <- currentDay . world
  long <- longitute .config
  lat <- latitude .config
  offset <- minutesToDiffTime `fmap` ( sunsetOffset . config)
  return $ utcTimeOfDay (addUTCTime offset (H.sunset day long lat))

-- Compute the sunrise time corresponding to the GlobalState
sunrise = do
  day <- currentDay . world
  long <- longitute . config
  lat <- latitude .config 
  offset <- minutesToDiffTime . (sunriseOffset . config)
  return $ utcTimeOfDay ( addUTCTime offset (H.sunset day long lat))



-- Check what is the expected door state according
-- to the current time and configuration
expectedDoorState :: GlobalState -> DoorState
expectedDoorState = do
  set <- sunset 
  rise <- sunrise 
  currentTime <- utcTimeOfDay `fmap` (currentTime . world)
  return $ if rise <= currentTime && currentTime <= set
             then DoorOpened
             else DoorClosed

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
