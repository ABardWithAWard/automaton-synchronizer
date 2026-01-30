module Main where

import Data.Time.Clock (diffUTCTime, getCurrentTime)

import AutomatonParser (DFA(..), State, Symbol)
import ShortestSynchronizingWordFinder (findSynchronizingWord)

--------------------------------------------------------------------------------
-- Cerny automaton generator
--------------------------------------------------------------------------------

-- Used it to test if it is possible to actually find the shortest word in hard cases.
-- Many automatons have a big lower bound for the shortest word which makes finding it hard.
-- Cerny automatons are one of classes of automatons where shortest synchronizing words are long.
-- Random DFAs are good to check if algorithm works, but the bigger the automaton the higher the probability its synchronizing. 
-- Most random DFAs have sublinear synchronizing words from what I noticed during testing as well, 
-- which makes finding shortest synchronizing word too easy to evaluate algorithm performance.
-- Cerny automatons also have a predictable lenght and are known to be:
-- (b^(n-1)a)^(n-2)b
-- so it is easy to verify solution even by hand.

-- Generates the Cerny automaton of size n
-- a: cyclic shift (i -> i+1 mod n)
-- b: only q0 moves to q1, others stay fixed
generateCernyDFA :: Int -> DFA
generateCernyDFA n = 
    let stateNames = ["q" ++ show i | i <- [0..n-1]]
        alphabetNames = ["a", "b"]
        
        -- Transition 'a': (q_i, a) -> q_{i+1 mod n}
        transA = [ (("q" ++ show i, "a"), "q" ++ show ((i + 1) `mod` n)) 
                 | i <- [0..n-1] ]
        
        -- Transition 'b': (q_0, b) -> q_1, else q_i -> q_i
        transB = (("q0", "b"), "q1") : 
                 [ (("q" ++ show i, "b"), "q" ++ show i) | i <- [1..n-1] ]
        
        allTrans = transA ++ transB
        
        -- Build transitionMat: Rows = States, Columns = Alphabet
        -- Row i corresponds to stateNames !! i
        -- Column j corresponds to alphabetNames !! j
        mat = [ [ step (stateNames !! i) (alphabetNames !! j) | j <- [0..1] ] 
              | i <- [0..n-1] ]
              where
                step s symb = case lookup (s, symb) allTrans of
                                Just s' -> s'
                                Nothing -> s -- shouldnt happen
    in DFA 
        { states        = stateNames
        , alphabet      = alphabetNames
        , startState    = "q0"
        , acceptStates  = ["q" ++ show (n-1)]
        , transitions   = allTrans
        , transitionMat = mat
        }

--------------------------------------------------------------------------------
-- Benchmark Runner
--------------------------------------------------------------------------------

runBenchmark :: Int -> IO ()
runBenchmark n = do
    let dfa = generateCernyDFA n
    let expectedLen = (n - 1) ^ (2 :: Int)

    putStrLn $ "Benchmarking Cerny DFA (n=" ++ show n ++ ")"
    
    start <- getCurrentTime
    
    let word = findSynchronizingWord dfa
    
    -- Force evaluation by printing the word BEFORE stopping the clock.
    -- This ensures the solver actually finished its work.
    -- When moved to other putStrLn compiler just doesnt evaluate word here and waits until show.
    putStrLn $ "Solver found word: " ++ show word
    
    end <- getCurrentTime
    
    let duration = diffUTCTime end start
    let foundLen = length word

    putStrLn $ "Found length:      " ++ show foundLen
    putStrLn $ "Target length:     " ++ show expectedLen
    putStrLn $ "Time elapsed:      " ++ show duration
    putStrLn "-----------------------------------------------\n"

main :: IO ()
main = do
    putStrLn "Starting benchmark...\n"
    mapM_ runBenchmark [3..15]