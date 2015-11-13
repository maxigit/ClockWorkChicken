-- | This module defines an interface to access Raspeberry PI GPIO.
-- The interface is meant to be substitute to real GPIO system
-- or mocked one (text-base or GUI).
module System.RaspberryPi where

data Pin = Pin1
         | Pin2
         | Pin3
         | Pin4
         | Pin5
         | Pin6
         | Pin7
         | Pin8
         | Pin9
         | Pin10
         | Pin11
         | Pin12
         | Pin13
         | Pin14
         | Pin15
         | Pin16
         deriving (Show, Read, Eq, Ord, Enum)

data Level = Low | High deriving (Show, Read, Eq, Ord, Enum)


data Pi m e = Pi
  { readPin :: Pin -> m Level
  , writePin :: Pin -> Level -> m()
  , extension :: e
  , run :: m () -> m ()
  }

