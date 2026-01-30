module ShortestSynchronizingWordFinder
  ( findSynchronizingWord
  ) where

import AutomatonParser (DFA(..), State, Symbol)

-- Finds the shortest synchronizing word using subset-based BFS.
-- Tried SAT solver as well, but it is unusable for Cerny automatons with n > 5.
-- This one can find the shortest word for Cerny automatons with given times:
-- n <= 10 <= 0.1s
-- n = 11 ~0.3s
-- n = 12 ~1s
-- n = 13 ~5s
-- n = 14 ~20s
-- n = 15 ~90s
-- n = 16 ~380s
-- n = 17 ~1660s
-- Tests were done using Benchmark.hs

-- Good way to visualize this algorithm is to check out Fig. 11 on page 20 (837) in pdf provided.
-- It works in a similiar way to algorithm which determinizes NFAs.
findSynchronizingWord :: DFA -> [Symbol]
findSynchronizingWord dfa =
    let
        sts  = states dfa
        syms = alphabet dfa
        n    = length sts

        -- Map states to indices
        stIdx s = getIdx s sts

        -- Transition table: transMap[u][s] = next state index
        transMap =
          [ [ stIdx next | next <- row ]
          | row <- transitionMat dfa
          ]

        -- Initial subset: all states
        startSubset = normalize [0 .. n - 1]

    in bfs syms transMap [(startSubset, [])] [startSubset]

--------------------------------------------------------------------------------
-- BFS over subsets
--------------------------------------------------------------------------------

type Subset = [Int]

-- Breadth-first search
-- DFS is easier to implement recursively, but BFS is necessary to ensure we get the shortest word.
bfs
  :: [Symbol]
  -> [[Int]]                 -- transition map
  -> [(Subset, [Symbol])]    -- queue: (current subset, word)
  -> [Subset]                -- visited subsets
  -> [Symbol]
bfs _ _ [] _ =
    []  -- should not happen

bfs syms transMap ((subset, word) : rest) visited
    -- If subset is a singleton, we are done
    | isSingleton subset =
        word

    | otherwise =
        let
            -- Generate successors by applying each symbol
            successors =
              [ let next = applySymbol transMap subset symIdx
                in (next, word ++ [sym])
              | (symIdx, sym) <- zip [0..] syms
              ]

            -- Filter out already visited subsets
            newOnes =
              [ (s, w)
              | (s, w) <- successors
              , s `notElem` visited
              ]

            newVisited = visited ++ map fst newOnes
            newQueue   = rest ++ newOnes

        in bfs syms transMap newQueue newVisited

--------------------------------------------------------------------------------
-- Subset operations
--------------------------------------------------------------------------------

-- Apply one symbol to a whole subset of states
applySymbol :: [[Int]] -> Subset -> Int -> Subset
applySymbol transMap subset symIdx =
    normalize
      [ transMap !! u !! symIdx
      | u <- subset
      ]

isSingleton :: Subset -> Bool
isSingleton [_] = True
isSingleton _   = False

-- Sort and remove duplicates
-- Needed not to duplicate nodes in graph, e.g. 01 is the same as 10
normalize :: Subset -> Subset
normalize = uniq . sort

-- Insertion sort
-- We do not sort really big arrays, so insertion is better than quicksort.
sort :: Ord a => [a] -> [a]
sort []     = []
sort (x:xs) = insert x (sort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insert y zs

-- Remove duplicates from sorted list
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/= x) xs)

-- Get ID of x in given list
getIdx :: Eq a => a -> [a] -> Int
getIdx = findIdx 0
  where
    findIdx _ _ [] = error "Element not found"
    findIdx i target (y:ys)
      | target == y = i
      | otherwise   = findIdx (i + 1) target ys