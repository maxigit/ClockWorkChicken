-- | Finite State machine.
module Data.Automaton where

import Control.Monad

-- data or type class ?
data Automaton m s ev = Automaton
  { exit :: s -> s -> m ()
  , enter :: Maybe s -> s -> m ()
  , transition :: s -> ev -> m s
  }

  

runAutomaton, runAutomaton' ::
  (Monad m, Eq s)
      => Automaton m s ev
      -> m (Maybe ev)
      -> s
      -> s
      -> m ()
runAutomaton autom em state endState = do
  enter autom Nothing state 
  runAutomaton' autom em state endState

runAutomaton' autom em state endState = do
  ev <- em
  newState <- case ev of 
    Nothing -> return state
    Just ev -> do
      newState <- transition autom state ev
      when (newState /= state) $ do
           exit autom state newState
           enter autom (Just state) newState
      return newState
  
  if newState == endState
    then return ()
    else runAutomaton' autom em newState endState

a = Automaton
    (\s s' -> putStrLn $ "exit from " ++ show s)
    (\s s' -> putStrLn $ "enter from " ++ show s')
    trans where
          trans x n = return $ x + n
  
