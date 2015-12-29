-- | IO version of Raspeberry PI mocked.
module System.RaspberryPi.Mock.IO
( System.RaspberryPi.Mock.IO.pi
)
where

import System.RaspberryPi
import System.RaspberryPi.Mock
import Control.Monad.State
import qualified Data.Map as Map
import Data.IORef

-- | Create a Pi mock reading and writing to map of pins and values (Mock)
-- we originaly used a stateMonad but as we already within a StateT
-- it's better to just store the Map as a global IORef

mockRef :: IO (IORef Mock)
mockRef = newIORef (Mock Map.empty)

pi :: e -> Pi IO e 
pi e = Pi read write e run where
  read pin = do
    mock <- readIORef =<< mockRef
    return $ readMockPin mock pin

  write pin level = do
    ref <- mockRef
    mock <- readIORef ref
    writeIORef ref mock { pins = (Map.insert pin level) (pins mock) }

  run m = mockRef >> m



