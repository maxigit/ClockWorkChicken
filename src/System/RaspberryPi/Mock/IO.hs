-- | IO version of Raspeberry PI mocked.
module System.RaspberryPi.Mock.IO where

import System.RaspberryPi
import System.RaspberryPi.Mock
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
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
    return $ fromMaybe Low ( Map.lookup pin (pins mock))

  write pin level = do
    ref <- mockRef
    mock <- readIORef ref
    writeIORef ref mock { pins = (Map.insert pin level) (pins mock) }

  run m = mockRef >> m

