-- | Finite State machine.
module Data.Automaton
( Automaton(..)
, Similar(..)
, runAutomaton
) where

import Control.Monad
import Data.Maybe

--  data or type class ?
--  | Describes an Automaton
data Automaton m s ev = Automaton
  { finished :: s -> Bool -- exitOn loop on 
  , enterState :: Maybe s -> s -> m () -- call when state change
  , exitState :: s -> s -> m () -- call when  state change
  , transition :: ev -> s -> s 
  , nextEvent :: m [ev] -- get the next events from the outside world
  }

  
-- | Like Eq but weaker. Allows different
-- state to be different but still similar.
class Similar a where
  (===) :: a -> a -> Bool
  a === b = not ( a /== b)
  (/==) :: a -> a -> Bool
  a /== b = not ( a === b)

runAutomaton :: (Monad m, Similar s) => Automaton m s ev -> s -> m s
runAutomaton autom state = do
  enterState autom Nothing state
  runAutomaton' autom state


runAutomaton' :: (Monad m, Similar s) => Automaton m s ev -> s -> m s
runAutomaton' autom state = do
  ev <- (nextEvent autom)
  -- @todo process all event if needed
  -- at the moment we just discard extra event
  newState <- case listToMaybe ev of
    Nothing -> return state
    Just ev -> do
      let newState = transition autom ev state 
      when (newState /== state) $ do
           exitState autom state newState
      return newState

  
  enterState autom (Just state) newState
  if finished autom newState
    then return newState
    else runAutomaton' autom newState

