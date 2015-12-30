module Main where

import CWC
import CWC.Mock
import Data.Automaton

import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime) 
import Data.Time (getCurrentTimeZone)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.IO.Class

import System.RaspberryPi
import System.RaspberryPi.Mock.IO as M

testPi' :: Pi IO (PiIO IO) 
testPi'  = Pi  
    readPin
    writePin
    mockPiIO
    (error "run not defined")
  where
    readPin pin = do
      putStrLn ("read pin #" ++ show pin)
      fmap read getLine
    readTime = do
      getCurrentTime
    writePin pin value = do
      putStrLn ("write pin #" ++ show pin ++ " -> " ++ show value)

testPi :: Pi IO (PiIO IO) 
testPi = M.pi mockPiIO
-- main :: IO GlobalState
main = do
  utc <- getCurrentTime
  zone <- getCurrentTimeZone

  let config = Config zone
                  (20/60) (53.75)
                  0 0 
      world = WorldState utc Low Low Low Low
      io = testPi
      global = GlobalState config world io

      initialPi = PiState Ajar  Ajar  TimeM
      enterSt = enterState automaton
      enterState' old new  = do
        liftIO $ putStrLn (replicate 50 '=')
        liftIO $ print new
        enterSt old new
        
      automaton' = automaton { enterState = enterState' }
  run io $ evalStateT (runAutomaton automaton' initialPi) global >> return ()
  return ()

