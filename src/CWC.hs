module CWC where

import Control.Concurrent (threadDelay)

data T 


-- * Misc Functions
-- | Loop the given action every n millisecond.
loop :: Int -> IO a -> IO a
loop n io = do
  threadDelay (1000*n)
  io
  loop n io
