module Main where

import System.RaspberryPi.GPIO
import Control.Concurrent (threadDelay)

main = withGPIO $ do
	setPinFunction Pin11 Output
	writePin Pin11 True
	threadDelay 1000000
	writePin Pin11 False
