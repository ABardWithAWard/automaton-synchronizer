module Main where

import AutomatonParser (parseDFAFromFile)
import PairGraphFallback (pairGraphFallback)
import ShortestSynchronizingWordFinder (findSynchronizingWord)

main :: IO ()
main = do
  putStrLn "Parsing automaton..."
  dfa <- parseDFAFromFile "automaton_input.txt"

  putStrLn "\nParsed DFA:"
  print dfa

  -- Dropped heuristics based synchronizability verification,
  -- as for larger automatas we cant find the shortest synchronizing word in a reasonable time anyway
  putStrLn "\nChecking synchronizability (Pair Graph)..."
  let isSynchronizable = pairGraphFallback dfa
  print isSynchronizable

  if isSynchronizable
    then do
      putStrLn "\nFinding shortest synchronizing word..."
      let word = findSynchronizingWord dfa
      putStrLn $ "\nSynchronizing Word: " ++ show word
    else 
      putStrLn "\nAutomaton is not synchronizable."