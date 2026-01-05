module BerlinkovMain
  ( shouldRunFallback
  ) where

import AutomatonParser (DFA)
import Aperiodic (isAperiodic)
import OneCluster (hasOneCluster)
import OneSink (hasOneSink)

-- Determines if we need to run the expensive O(n^2) Pair Graph algorithm.
--
-- LOGIC EXPLANATION:
-- We check for "Sufficient Conditions" for synchronization.
-- If an automaton satisfies these structural properties, it is mathematically 
-- guaranteed to be synchronizing. Therefore, we do NOT need the fallback.
--
-- 1. One Sink: If there is a reachable state that transitions to itself on all 
--    inputs, the automaton synchronizes to that state.
-- 2. Aperiodic & One Cluster: By the Road Coloring Theorem and subsequent 
--    probabilistic proofs, a strongly connected (One Cluster) and Aperiodic 
--    graph is synchronizing.

shouldRunFallback :: DFA -> Bool
shouldRunFallback dfa = not (isDefiniteSuccess dfa)

isDefiniteSuccess :: DFA -> Bool
isDefiniteSuccess a = hasOneSink a || (isAperiodic a && hasOneCluster a)