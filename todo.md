#  Automate
Real world
config
internal state

Event

? :: World -> Internal -> Event
  can be done with a state monad
  Internal -> m Event


read :: m World
ActionIn = Read ft
    | getTime

readIn :: Int ->  m Int


write :: World -> M()
We need to abstract "World" to be able to
switch betwen Pi and manual 
operation reading world
  getCurrentTime
  get door status -> read pin

writing
  write pin
  or writePin :: Int -> IO () or Int -> m ()
  Action type ... Action = WritePin 

  Action -> IO () -- print
  Action -> IO () -- raspebbery
                         
                         
 WorldModule mi mo = WorldModule
  readPin :: Int -> mi Float
  getCurrentTime :: mi Time
  
  writePin :: Int -> Float -> mo ()

How to integrate it in our automaton ?

?? :: Internal -> mi Event
transitions :: Internal -> Internal  -> mo ()

how to link mo and mi




