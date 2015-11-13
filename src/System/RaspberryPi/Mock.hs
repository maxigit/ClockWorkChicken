-- | State representing a "mocked" raspberry pi .
-- base for Text-base
module System.RaspberryPi.Mock where

import Data.Map (Map)
import System.RaspberryPi

data Mock = Mock 
  { pins :: Map Pin Level
  }


