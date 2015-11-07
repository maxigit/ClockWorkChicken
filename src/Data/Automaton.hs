-- | Finite State machine.
module Data.Automaton where

import Control.Monad

-- data or type class ?
data Automaton m s ev = Automaton
  { exit :: s -> s -> m ()
  , enter :: Maybe s -> s -> m ()
  , transition :: s -> ev -> m s
  }

  

-- runAutomaton :: MonadPlus m =>  Automaton m s ev -> s -> m ()
runAutomaton autom evs state = do
  enter autom Nothing state 
  runAutomaton' autom evs state

runAutomaton' autom [] state = do
  return ()

runAutomaton' autom (ev:evs) state = do
  newState <- transition autom state ev
  when (newState /= state) $ do
                          exit autom state newState
                          enter autom (Just state) newState
  
  runAutomaton' autom evs newState

a = Automaton
    (\s s' -> putStrLn $ "exit from " ++ show s)
    (\s s' -> putStrLn $ "enter from " ++ show s')
    trans where
          trans x n = return $ x + n
  
