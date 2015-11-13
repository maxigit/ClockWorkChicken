-- | IO version of Raspeberry PI mocked.
module System.RaspberryPi.Mock.IO where

import System.RaspberryPi
import System.RaspberryPi.Mock
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

pi :: e -> Pi (StateT Mock IO) e 
pi e = Pi read write e run where
  read pin = fmap (fromMaybe Low . Map.lookup pin) (gets pins) 

  write pin level = modify mod where
    mod mock = mock { pins = (Map.insert pin level) (pins mock) }
  run m = do
    let s = execStateT m state
        state = Mock Map.empty
    return ()

