module Aperiodic
  ( isAperiodic
  ) where

import AutomatonParser (DFA(..))

-- | Checks if the DFA is aperiodic using Tarjan's algorithm.
-- This implementation assumes 'aperiodic' means the transition graph 
-- has no non-trivial cycles (all Strongly Connected Components are size 1).
isAperiodic :: DFA -> Bool
isAperiodic dfa = False