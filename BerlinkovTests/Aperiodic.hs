module Aperiodic
  ( isAperiodic
  ) where

import AutomatonParser (DFA(..), State)

isAperiodic :: Eq State => DFA -> Bool
isAperiodic dfa = all isTrivialSCC (findSCCs (states dfa) (transitions dfa))
  where
    -- A trivial SCC is just a single state.
    -- If an SCC has > 1 state, it contains a cycle that permutes states.
    isTrivialSCC :: [State] -> Bool
    isTrivialSCC [_] = True
    isTrivialSCC []  = True
    isTrivialSCC _   = False

-- Tarjan's Algorithm State
data TarjanState = TS 
  { discovery :: [(State, Int)] -- Time a state was first visited
  , lowlink   :: [(State, Int)] -- Lowest discovery time reachable
  , stack     :: [State]        -- Current nodes in recursion
  , time      :: Int            -- Global clock
  , sccs      :: [[State]]      -- Resulting components
  }

findSCCs :: Eq State => [State] -> [((State, symbol), State)] -> [[State]]
findSCCs allStates trans = sccs $ foldl (visit trans) initialState allStates
  where
    initialState = TS [] [] [] 0 []

    visit :: Eq State => [((State, symbol), State)] -> TarjanState -> State -> TarjanState
    visit ts st u
      | u `lookup` discovery st /= Nothing = st
      | otherwise = 
          let 
            -- Initialize state for new node
            st0 = st { discovery = (u, time st) : discovery st
                     , lowlink   = (u, time st) : lowlink st
                     , stack     = u : stack st
                     , time      = time st + 1 
                     }
            
            -- Visit neighbors
            neighbors = [ v | ((s, _), v) <- ts, s == u ]
            st1 = foldl (dfs ts u) st0 neighbors
            
            -- Check if 'u' is the root of an SCC
            uDisc = lookup' u (discovery st1)
            uLow  = lookup' u (lowlink st1)
          in 
            if uDisc == uLow
            then 
              let (comp, newStack) = spanUntil u (stack st1)
              in st1 { stack = newStack, sccs = comp : sccs st1 }
            else st1

    dfs ts u st v =
      case lookup v (discovery st) of
        Nothing -> -- v not visited, recurse
          let st' = visit ts st v
              newLow = min (lookup' u (lowlink st')) (lookup' v (lowlink st'))
          in st' { lowlink = update u newLow (lowlink st') }
        _ -> -- v visited
          if v `elem` stack st
          then let newLow = min (lookup' u (lowlink st)) (lookup' v (discovery st))
               in st { lowlink = update u newLow (lowlink st) }
          else st

    -- Helper functions for manual state management
    lookup' k m = case lookup k m of { Just x -> x; Nothing -> error "State missing" }
    update k v m = (k, v) : filter ((/= k) . fst) m
    spanUntil x (y:ys) | x == y    = ([y], ys)
                       | otherwise = let (c, s) = spanUntil x ys in (y:c, s)