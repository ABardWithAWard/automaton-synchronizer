module BerlinkovMain
  ( runBerlinkovTests
  ) where

import AutomatonParser (DFA)
import Aperiodic
import OneCluster
import OneSink

runBerlinkovTests :: DFA -> IO ()
runBerlinkovTests dfa = do
  putStrLn "\n[Berlinkov] Running Aperiodic test"
  print (isAperiodic dfa)

  putStrLn "\n[Berlinkov] Running One-Cluster test"
  print (hasOneCluster dfa)

  putStrLn "\n[Berlinkov] Running One-Sink test"
  print (hasOneSink dfa)
