-- | Defines Mock for interactive testing
module CWC.Mock
( mockPiIO
) where
import CWC
import Data.Char(toLower)
import Data.Maybe
import Control.Monad.State
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime) 
import System.IO

import System.RaspberryPi
-- | 
mockPiIO ::  PiIO IO 
mockPiIO = (piIO :: PiIO IO)
  { readWorld = mockReadWorld
  , displayWorld = mockDisplayWorld
  , openDoor = mockOpenDoor
  , closeDoor = mockCloseDoor
  , lockDoor = mockLockDoor
  , unlockDoor = mockUnlockDoor
  , displayTime = mockDisplayTime
  }

mockReadWorld :: GState IO WorldState
mockReadWorld    = do
  oldWorld <- gets world
  liftIO $ putStrLn "press ? or h for help"
  let actions = [('6', "Advance clock by 6 ", advanceClock 6)
                ,('1', "Advance clock by 6 ", advanceClock 1)
                ,('o', "Door opened", undefined)
                ,('c', "Door closed", undefined)
                ]
      advanceClock hour = do
          let time = currentTime oldWorld
              newTime = addUTCTime (3600*hour) time 
          return $ oldWorld {currentTime = newTime }
  newWorld <- menu actions
  return $ fromMaybe oldWorld newWorld  


-- | Display all mode with a * in front of the current mode
mockDisplayWorld world mode = do
  let modes = [minBound..maxBound]
  displays <- mapM displayFromState modes
  let format s m | m == mode = "*** " ++ s ++ " ***"
                 | otherwise = "    " ++ s ++ "    "

  liftIO $ do
    putStrLn (replicate 38 '-')
    mapM_ putStrLn (zipWith format displays modes)
  displayPins

displayPins = do
  io <- gets io
  liftIO $ do 
    values <-  mapM (readPin io) [minBound..maxBound]
    let toChar Low = '_'
        toChar High = '^'
    putStrLn (map toChar values)

  
  

callDefault message action = lift (putStrLn message' ) >> action piIO
  where message' = "===========> " ++ message

mockOpenDoor     = callDefault "opening door" openDoor
mockCloseDoor    = callDefault "closing door" closeDoor
mockLockDoor     = callDefault "locking door" lockDoor
mockUnlockDoor   = callDefault "unlocking door" unlockDoor
mockDisplayTime  = error "todo :displayTime"

-- * Menus
-- | Display a list of actions and select the one
-- corresponding to the key
menu actions = do
  bufMode <- liftIO $ hGetBuffering stdin
  liftIO $ hSetBuffering stdin NoBuffering
  c <- liftIO getChar
  liftIO $ hSetBuffering stdin bufMode
  liftIO $ putStrLn ""
  case c of 
    'h' -> displayHelp
    '?' -> displayHelp
    _ -> executeAction c

  where displayHelp = liftIO $ do
          mapM_ (\(k,message,_) -> putStrLn $ k:" - " ++ message)
                (('h', "this message", undefined) : actions)
          return Nothing
        executeAction c = case lookup c actions' of
          Nothing -> return Nothing
          Just action -> fmap Just action 
          where actions' = map (\(c,_,a) -> (c,a)) actions

