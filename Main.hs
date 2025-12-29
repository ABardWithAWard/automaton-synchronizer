module Main where

import AutomatonParser
import PairGraphFallback
import BerlinkovMain

main :: IO ()
main = do
  putStrLn "Parsing automaton"
  dfa <- parseDFAFromFile "automaton_input.txt"

  putStrLn "\nParsed DFA:"
  print dfa

  putStrLn "\nRunning Berlinkov tests"
  runBerlinkovTests dfa

  putStrLn "\nRunning fallback test"
  let isSynchronizable = pairGraphFallback dfa

  putStrLn "\nIs Synchronizable?"
  print isSynchronizable
