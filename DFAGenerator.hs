module Main where

import System.Random (randomRIO)

import AutomatonParser (DFA(..), State, Symbol)
import PairGraphFallback (pairGraphFallback)
import ShortestSynchronizingWordFinder (findSynchronizingWord)

-- replicateM: runs an IO action n times and collects results
replicateIO :: Int -> IO a -> IO [a]
replicateIO 0 _ = return []
replicateIO n action = do
    x <- action
    xs <- replicateIO (n - 1) action
    return (x:xs)

-- nub: removes duplicate elements from a list
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)

generateRandomDFA :: Int -> Int -> IO DFA
generateRandomDFA n m = do
    let stateNames = ["q" ++ show i | i <- [0..n-1]]
    let alphabetNames = [[toEnum (97 + i)] | i <- [0..m-1]] -- a, b, c, etc.
    
    -- Generate random transitions for the matrix
    mat <- replicateIO n (replicateIO m (pickRandom stateNames))
    
    let transList = [ ((stateNames !! i, alphabetNames !! j), (mat !! i) !! j) 
                    | i <- [0..n-1], j <- [0..m-1] ]
    
    return $ DFA 
        { states = stateNames
        , alphabet = alphabetNames
        , startState = "q0"
        , acceptStates = ["q" ++ show (n-1)]
        , transitions = transList
        , transitionMat = mat
        }

formatDFA :: DFA -> String
formatDFA dfa =
    "states: " ++ unwords (states dfa) ++ "\n" ++
    "alphabet: " ++ unwords (alphabet dfa) ++ "\n" ++
    "start: " ++ startState dfa ++ "\n" ++
    "accepting: " ++ unwords (acceptStates dfa) ++ "\n" ++
    "transitions:\n" ++ concatMap formatTrans (transitions dfa)
  where
    formatTrans ((s, symb), target) = s ++ " " ++ symb ++ " " ++ target ++ "\n"

pickRandom :: [a] -> IO a
pickRandom xs = do
    idx <- randomRIO (0, length xs - 1)
    return (xs !! idx)

--------------------------------------------------------------------------------
-- Verification logic
--------------------------------------------------------------------------------

-- Simulates a word on a set of states to see the resulting subset
simulate :: DFA -> [State] -> [Symbol] -> [State]
simulate _ currSet [] = unique currSet
simulate dfa currSet (w:ws) = 
    let nextSet = [ step s w | s <- currSet ]
    in simulate dfa nextSet ws
  where
    step s symb = case lookup (s, symb) (transitions dfa) of
                    Just s' -> s' -- returns destination state
                    Nothing -> s -- shouldnt happen

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

runTest :: Int -> IO ()
runTest testNum = do
    putStrLn $ "### TEST " ++ show testNum ++ " ###"
    
    dfa <- generateRandomDFA 20 3
    putStrLn (formatDFA dfa)
    
    let isSync = pairGraphFallback dfa
    putStrLn $ "Pair graph says synchronizable: " ++ show isSync
    
    if isSync 
        then do
            let word = findSynchronizingWord dfa
            putStrLn $ "Result: " ++ show word
            
            let finalStates = simulate dfa (states dfa) word
            putStrLn $ "Verification: " ++ show finalStates
            if length finalStates == 1
                then putStrLn "Result: VALID"
                else putStrLn "Result: INVALID"
        else 
            putStrLn "Result: Non-synchronizable"
    putStrLn "-------------------------------\n"

main :: IO ()
main = mapM_ runTest [1..3]