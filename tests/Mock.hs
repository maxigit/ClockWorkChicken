module Mock (
testPi
) where

import CWC
import System.RaspberryPi
import Data.IORef
import System.IO.Unsafe
import Control.Monad.IO.Class

statesRef :: IORef [WorldState]
{-# NOINLINE statesRef #-}
statesRef = unsafePerformIO $ newIORef []

mockPiIO ::  PiIO IO 
mockPiIO = (piIO :: PiIO IO)
  { readWorld = mockReadWorld
  -- , displayWorld = const $ return ()
  -- , execute = const $ return ()
  -- , displayTime = return ()
  }


mockReadWorld :: GState IO WorldState
mockReadWorld = do
  states <- liftIO $ readIORef statesRef
  case states of
    [] -> error "Not enough states supplied"
    (st:sts) -> do
      liftIO $ writeIORef statesRef sts
      return st


-- | Display all mode with a * in front of the current mode
mockDisplayWorld world mode = return ()

-- testPi :: Pi IO 
testPi states  = Pi (const $ return Low) (const $const $ return ()) mockPiIO run where
  run m = writeIORef statesRef states >> m

    
    
