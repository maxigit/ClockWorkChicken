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

data DayNight = Daytime | Nighttime deriving (Show, Read, Eq)

-- | Input coming from the outside world
-- hold current time, and the state of different sensors
data WorldState = WorldState 
  { currentTime :: UTCTime
  , opennedDoorSensor :: Level
  , closedDoorSensor :: Level
  , doorLockedSensor :: Level
  , doorUnlockedSensor :: Level
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
  
  
sunset, sunrise :: Config -> WorldState -> TimeOfDay
-- Compute the sunset time corresponding to the GlobalState
sunset conf world = let
  day = currentDay world
  long = longitute conf
  lat = latitude conf
  offset = minutesToDiffTime (sunsetOffset conf)
  in utcTimeOfDay (addUTCTime offset (H.sunset day long lat))

-- Compute the sunrise time corresponding to the GlobalState
sunrise conf world = let
  day = currentDay world
  long = longitute conf
  lat = latitude conf
  offset = minutesToDiffTime (sunriseOffset conf)
  in utcTimeOfDay (addUTCTime offset (H.sunrise day long lat))



-- Check what is the expected door state according
-- to the current time and configuration
dayNight :: Config -> WorldState -> DayNight
dayNight conf world = let
  set = sunset conf world
  rise = sunrise conf world
  time = utcTimeOfDay (currentTime world)
  in if rise <= time && time <= set
             then Daytime
             else Nighttime

-- | lift a config/world function to a GState 
liftG :: Monad m => (Config -> WorldState -> a) ->  GState m a
liftG f = do
  conf <- gets config
  world <- gets world

  return $ f conf world

displayFromState :: Monad m => DisplayMode ->  GState  m String
displayFromState TimeM = liftM show $ gets (currentTime.world)
displayFromState SunriseM = liftM (("^ "++) . show) (liftG sunrise)
displayFromState SunsetM = liftM (("v "++) . show) (liftG sunset)
displayFromState LongitudeM = liftM (("Lo "++) . show) $ gets (longitute.config)
displayFromState LatitudeM = liftM (("La "++) . show) $ gets (latitude.config)


-- * Commands
-- | Possible commands to drive the RaspberryPi (open/close) door  etct
data MotorState = Forward | Backward | Stop deriving (Show, Read, Eq, Enum, Bounded)
data Command = DoorMotor MotorState
             | LockMotor MotorState
             deriving (Show, Read, Eq)
--
-- * In
-- * Pi Record implementing the processing of the command
-- allows to switch between real Raspberry Pi and different Mock
data PiIO m = PiIO
  { readWorld :: GState m WorldState
  , displayWorld :: WorldState -> DisplayMode -> GState m ()
  , execute :: Command -> GState m ()
  , displayTime :: UTCTime -> GState m ()
  }

-- | Default value for PiIO . Can be seen as the base class
openDoorPin = Pin1
closeDoorPin = Pin2
lockDoorPin = Pin3
unlockDoorPin = Pin4
doorClosedPin = Pin5
doorOpenedPin = Pin6
lockClosedPin = Pin7
lockOpenPin = Pin8

piIO :: Monad m => PiIO m
piIO = PiIO rw dw ex dt where
     rw = do
      world <- gets world
      io <- gets io
      lift $ do
        dop <- readPin io doorOpenedPin 
        dc <- readPin io doorClosedPin
        lo <- readPin io lockOpenPin
        lc <- readPin io lockClosedPin

        return $ world { opennedDoorSensor = dop
                     , closedDoorSensor = dc
                     , doorLockedSensor = lc
                     , doorUnlockedSensor = lo
                     }
      
     dw = error "displayWorld not implemented"
     ex command = do
      io <- gets io
      lift $ case command of
        DoorMotor Forward -> writePin io openDoorPin High  >> writePin io closeDoorPin Low
        DoorMotor Stop -> writePin io openDoorPin Low  >> writePin io closeDoorPin Low
        DoorMotor Backward -> writePin io openDoorPin Low  >> writePin io closeDoorPin High
        LockMotor Forward -> writePin io lockDoorPin High  >> writePin io unlockDoorPin Low
        LockMotor Stop -> writePin io lockDoorPin Low  >> writePin io unlockDoorPin Low
        LockMotor Backward -> writePin io lockDoorPin Low  >> writePin io unlockDoorPin High
     dt = error "displayTme not implemented"


-- * Out
out :: PiState -> PiState -> IO ()
out _ _ = return ()

-- | Analyse a world state and generate an event
step :: Config -> WorldState -> WorldState -> Maybe Event
step conf old new = let
  oldDayNight = dayNight conf old
  newDayNight = dayNight conf new

  in case (oldDayNight, newDayNight) of
    (Nighttime, Daytime) -> Just Sunrise
    (Daytime, Nighttime) -> Just Sunset
    _ -> Nothing
    

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
    go old new
    (displayWorld ex) (worldState) (displayMode new)

    where go Nothing _ = return ()
          go _ state | doorState state == Opening    = exec (DoorMotor Forward)
                     | doorState state == DoorOpened = exec (DoorMotor Stop)
                     | doorState state == Closing    = exec (DoorMotor Backward)
                     | doorState state == DoorClosed = exec (DoorMotor Stop)
          go _ _ = return ()
          exec command = do
            ext <- gets (extension . io)
            (execute ext) command


  exitState _ _ = return () -- @todo

  nextEvent = do
    global <- get
    let oldWorld = world global
    ex <- gets (extension.io)
    newWorld <- readWorld ex
    put global { world =  newWorld }
    return (step (config global) oldWorld newWorld)

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

  transition ev state = error $ "Not transiton for "
                              ++ show  ev
                              ++ " from "
                              ++ show state


    
    
    
  
  


-- * Misc Functions
-- | Loop the given action every n millisecond.
loop :: Int -> (a -> IO a) -> a -> IO a
loop n f state = do
  threadDelay (1000*n)
  state'  <- f state
  loop n f state'
