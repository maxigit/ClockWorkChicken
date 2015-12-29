-- | Defines Mock for interactive testing
module CWC.Mock
( mockPiIO
) where
import CWC
import Data.Char(toLower)
import Control.Monad.State
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime) 
-- | 
mockPiIO ::  PiIO IO 
mockPiIO = PiIO mockReadWorld
              mockDisplayWorld
              mockOpenDoor
              mockCloseDoor
              mockLockDoor
              mockUnlockDoor
              mockDisplayTime

mockReadWorld :: GState IO WorldState
mockReadWorld    = do
  oldWorld <- gets world
  liftIO $ print "do you want to increase time?"
  answer <- liftIO getLine
  if (map toLower answer) `elem` ["y", "yes"]
     then let time = currentTime oldWorld
              newTime = addUTCTime (3600*6) time 
          in return oldWorld {currentTime = newTime }
          

     else return oldWorld

-- | Display all mode with a * in front of the current mode
mockDisplayWorld world mode = do
  let modes = [minBound..maxBound]
  displays <- mapM displayFromState modes
  let format s m | m == mode = "*** " ++ s ++ " ***"
                 | otherwise = "    " ++ s ++ "    "

  liftIO $ do
    putStrLn (replicate 38 '-')
    mapM_ putStrLn (zipWith format displays modes)


mockOpenDoor     = error "todo :openDoor"
mockCloseDoor    = error "todo :closeDoor"
mockLockDoor     = error "todo :lockDoor"
mockUnlockDoor   = error "todo :unlockDoor"
mockDisplayTime  = error "todo :displayTime"

