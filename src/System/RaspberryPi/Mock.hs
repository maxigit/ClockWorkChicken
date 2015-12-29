-- | State representing a "mocked" raspberry pi .
-- base for Text-base
module System.RaspberryPi.Mock where

import Data.Map (Map)
import qualified Data.Map as Map
import System.RaspberryPi
import Data.Maybe (fromMaybe)

data Mock = Mock 
  { pins :: Map Pin Level
  }

readMockPin :: Mock -> Pin -> Level
readMockPin mock pin = fromMaybe Low (Map.lookup pin (pins mock))



