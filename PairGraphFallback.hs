module PairGraphFallback
  ( pairGraphFallback
  ) where

import AutomatonParser (DFA(..), State, Symbol)

-- Checks if the DFA is synchronizable by verifying every pair of states is mergeable.
pairGraphFallback :: DFA -> Bool
pairGraphFallback dfa = 
    let allStates = states dfa
        -- Generate all unique unordered pairs {s1, s2} where s1 /= s2
        pairsToTest = [ (s1, s2) | s1 <- allStates, s2 <- allStates, s1 < s2 ]
    in all (\p -> canMerge p [] dfa) pairsToTest

-- DFS to find if a pair can reach a state (s, s)
canMerge :: (State, State) -> [(State, State)] -> DFA -> Bool
canMerge (u, v) visited dfa
    | u == v = True
    | isVisited (u, v) visited = False
    | otherwise = 
        let nextVisited = (u, v) : visited
            syms = alphabet dfa
            -- Check if any symbol leads to a path that merges
            results = [ canMerge (getNext u sym dfa, getNext v sym dfa) nextVisited dfa 
                      | sym <- syms ]
        in any id results

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- Checks if a pair (or its reverse) has been visited
isVisited :: (State, State) -> [(State, State)] -> Bool
isVisited _ [] = False
isVisited (u, v) ((v1, v2):vs)
    | (u == v1 && v == v2) || (u == v2 && v == v1) = True
    | otherwise = isVisited (u, v) vs

-- Looks up the transition for a state and symbol
getNext :: State -> Symbol -> DFA -> State
getNext s sym dfa = 
    case findTransition (transitions dfa) s sym of
        Just s' -> s'
        Nothing -> s -- Default to self if transition is not defined, but automatas should be fully defined
  where
    findTransition [] _ _ = Nothing
    findTransition (((st, sy), target):ts) s symbol
        | st == s && sy == symbol = Just target
        | otherwise = findTransition ts s symbol