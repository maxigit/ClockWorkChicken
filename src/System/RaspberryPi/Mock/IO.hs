-- | IO version of Raspeberry PI mocked.
module System.RaspberryPi.Mock.IO
( System.RaspberryPi.Mock.IO.pi
)
where

import System.IO.Unsafe
import System.RaspberryPi
import System.RaspberryPi.Mock
import Control.Monad.State
import qualified Data.Map as Map
import Data.IORef

-- | Create a Pi mock reading and writing to map of pins and values (Mock)
-- we originaly used a stateMonad but as we already within a StateT
-- it's better to just store the Map as a global IORef

mockRef :: IORef Mock
{-# NOINLINE mockRef #-}
mockRef = unsafePerformIO $ newIORef (Mock Map.empty)

pi :: e -> Pi IO e 
pi e = Pi read write e run where
  read pin = do
    mock <- readIORef mockRef
    return $ readMockPin mock pin

  write pin level = do
    mock <- readIORef mockRef
    writeIORef mockRef mock { pins = (Map.insert pin level) (pins mock) }
    Mock pins <- readIORef mockRef
    putStrLn $ "---> write " ++ show pin ++ ":" ++  show level

  run m = return mockRef >> m



