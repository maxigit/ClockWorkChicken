-- | Finite State machine.
module Data.Automaton where

import Control.Monad

-- data or type class ?
data Automaton m s ev = Automaton
  { initial :: s -- initial state
  , finished :: s -> Bool -- exitOn loop on 
  , enterState :: s -> s -> m ()
  , exitState :: Maybe s -> s -> m ()
  , transition :: s -> ev -> m s
  , nextEvent :: m (Maybe  ev)
  }

  
class Similar a where
  (===) :: a -> a -> Bool
  a === b = not ( a /== b)
  (/==) :: a -> a -> Bool
  a /== b = not ( a === b)

runAutomaton :: (Monad m, Similar s) => Automaton m s ev -> s -> m s
runAutomaton autom state = do
  exitState autom Nothing state
  runAutomaton' autom state


runAutomaton' :: (Monad m, Similar s) => Automaton m s ev -> s -> m s
runAutomaton' autom state = do
  ev <- (nextEvent autom)
  newState <- case ev of 
    Nothing -> return state
    Just ev -> do
      newState <- transition autom state ev
      when (newState /== state) $ do
           enterState autom state newState
           exitState autom (Just state) newState
      return newState
  
  if finished autom newState
    then return newState
    else runAutomaton' autom newState

