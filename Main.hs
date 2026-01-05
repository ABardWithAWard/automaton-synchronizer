module Main where

import AutomatonParser (parseDFAFromFile)
import PairGraphFallback (pairGraphFallback)
import BerlinkovMain (shouldRunFallback)

main :: IO ()
main = do
  putStrLn "Parsing automaton..."
  dfa <- parseDFAFromFile "automaton_input.txt"

  putStrLn "\nParsed DFA:"
  print dfa

  putStrLn "\nDetermining synchronizability..."

  -- LOGIC FLOW:
  -- 1. Ask Berlinkov module if we need to run the expensive test.
  -- 2. If False: The heuristics proved it is synchronizing.
  -- 3. If True: The heuristics were unsure, so we run the Pair Graph Fallback.

  let isSynchronizable =
        if shouldRunFallback dfa
          then pairGraphFallback dfa
          else True

  putStrLn "\nIs Synchronizable?"
  print isSynchronizable