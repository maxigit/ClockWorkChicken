module Main where

import System.Hardware.GPIO.Pin as P
import Control.Concurrent (threadDelay)


testGPIO :: Int -> IO ()
testGPIO pin = do
	p <- P.init pin Out
	v1 <- P.read p
	putStrLn $ "Pin " ++ show pin ++ ": " ++ show v1
	P.set p P.One
	threadDelay 1000000
	v2 <- P.read p
	P.set p P.Zero
	putStrLn $ "Pin " ++ show pin ++ ": " ++ show v2
	P.close p

main :: IO ()
main = do
	mapM_ testGPIO [17]
