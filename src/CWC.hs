module CWC where

import System.RaspberryPi
import Control.Concurrent (threadDelay)
import Data.Time
import qualified Data.Time.Horizon as H
import Data.Automaton

import Control.Monad.State

-- * State
-- | The state (actual or desired) of a door
data OpenClose = Opened | Closed
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

nextDisplayMode :: DisplayMode -> DisplayMode
nextDisplayMode mode | mode == maxBound = minBound
nextDisplayMode mode = succ mode
-- | Event to react on
-- Open/Close are command send by the user.
-- An open door, will stay open until a sunrise even occurs.
data Event = Sunrise 
           | Sunset 
           | OpenDoor -- ^ Overwride door status
           | CloseDoor
           | Door OpenClose -- ^ Door state changes
           | NextDisplayMode
     deriving (Show, Eq)

-- | Abstract state of the PI
-- will be converted in GPIO action
data DoorState = DoorClosed
             | Opening
             | DoorOpened
             | Closing
     deriving (Show, Read, Eq)

data PiState = PiState
  { doorState ::   DoorState 
  , lockState :: DoorState
  , displayMode :: DisplayMode
  } deriving (Show, Read, Eq)


-- | Miscellaneous configuration
data Config = Config
  { timeZone :: TimeZone
  , longitute :: H.LongitudeWest
  , latitude :: H.LatitudeNorth
  , sunsetOffset :: Integer
  , sunriseOffset :: Integer
  } deriving (Show, Read)

-- | State combining everything
data GlobalState m = GlobalState 
  { config :: Config
  , world :: WorldState
  , io :: Pi m (PiIO m)
  } 

type GState m = StateT (GlobalState m) m

instance Similar PiState where
  a === b = a == b

-- * Time related functions

utcZone = hoursToTimeZone 0
minutesToDiffTime = fromInteger . (60*)

currentDay :: WorldState ->  Day
currentDay = fmap localDay (utcToLocalTime utcZone .currentTime)

utcTimeOfDay :: UTCTime -> TimeOfDay
utcTimeOfDay = localTimeOfDay . utcToLocalTime utcZone
  
  
sunset, sunrise :: Monad m => GState  m TimeOfDay
-- Compute the sunset time corresponding to the GlobalState
sunset = do
  day <- gets (currentDay . world)
  long <- gets (longitute .config)
  lat <- gets (latitude .config)
  offset <- minutesToDiffTime `liftM` (gets $ sunsetOffset . config)
  return $ utcTimeOfDay (addUTCTime offset (H.sunset day long lat))

-- Compute the sunrise time corresponding to the GlobalState
sunrise = do
  day <- gets (currentDay . world)
  long <- gets (longitute .config)
  lat <- gets (latitude .config)
  offset <- minutesToDiffTime `liftM` (gets $ sunriseOffset . config)
  return $ utcTimeOfDay (addUTCTime offset (H.sunrise day long lat))



-- Check what is the expected door state according
-- to the current time and configuration
expectedDoorState :: Monad m => GState m OpenClose
expectedDoorState = do
  set <- sunset 
  rise <- sunrise 
  currentTime <- utcTimeOfDay `liftM` (gets $ currentTime . world)
  return $ if rise <= currentTime && currentTime <= set
             then Opened
             else Closed


displayFromState :: Monad m => DisplayMode ->  GState  m String
displayFromState TimeM = liftM show $ gets (currentTime.world)
displayFromState SunriseM = liftM (("^ "++) . show) $ sunrise
displayFromState SunsetM = liftM (("v "++) . show) $ sunset
displayFromState LongitudeM = liftM (("Lo "++) . show) $ gets (longitute.config)
displayFromState LatitudeM = liftM (("La "++) . show) $ gets (latitude.config)


-- * In
-- * Pi IO extension
data PiIO m = PiIO
  { readWorld :: GState m WorldState
  , displayWorld :: WorldState -> DisplayMode -> GState m ()
  , openDoor :: GState m ()
  , closeDoor :: GState m ()
  , lockDoor :: GState m ()
  , unlockDoor :: GState m ()
  , displayTime :: UTCTime -> GState m ()
  }

-- * Out
out :: PiState -> PiState -> IO ()
out _ _ = return ()

-- | Analyse a world state and generate an event
step :: WorldState -> WorldState -> Maybe Event
step old new = Nothing

-- * Automaton

automaton :: Monad m => Automaton (GState m) PiState Event
automaton = Automaton exitOn
                      enterState 
                      exitState
                      transition
                      nextEvent where
  exitOn s = False
  enterState old new = do
    worldState <- gets world
    ex <- gets (extension.io)
    (displayWorld ex) (worldState) (displayMode new)
    go old new

    where go Nothing _ = return ()
          go _ state | doorState state == Opening
                     = gets (extension.io) >>= openDoor
          go _ state | doorState state == Closing
                     = gets (extension.io) >>= closeDoor
          go _ _ = return ()

  exitState _ _ = return () -- @todo

  nextEvent = do
    global <- get
    let oldWorld = world global
    ex <- gets (extension.io)
    newWorld <- readWorld ex
    put global { world =  newWorld }
    return (step oldWorld newWorld)

  transition ev state
    | ev `elem` [Sunrise, OpenDoor]
       && doorState state `elem` [DoorClosed, Closing]
       = state {  doorState = Opening }

    | ev `elem` [Sunset, CloseDoor]
       && doorState state `elem` [DoorOpened, Opening]
       = state {  doorState = Closing }

  transition (Door Closed) state = state { doorState = DoorClosed }
  transition (Door Opened) state = state { doorState = DoorOpened }

  transition NextDisplayMode state = state { displayMode = nextDisplayMode (displayMode state) }


    
    
    
  
  


-- * Misc Functions
-- | Loop the given action every n millisecond.
loop :: Int -> (a -> IO a) -> a -> IO a
loop n f state = do
  threadDelay (1000*n)
  state'  <- f state
  loop n f state'
