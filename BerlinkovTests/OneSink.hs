module OneSink
  ( hasOneSink
  ) where

import AutomatonParser (DFA, transitionMat, states)
type State = String
type Symbol = String

--if there is MAXIMUM one sink we should accept the automaton, 
--since automatas with two sinks (or more) arent synchronizable
--should be implemented using this to get linear time in regard to nodes
--https://wiki.c2.com/?GraphSinkDetection
--as of now i just check all edges so still linear but in regard to edges so N^2 runtime xd
hasOneSink :: DFA -> Bool
hasOneSink dfa = 1 >= length (filter id [ isSink s row | (s, row) <- zip (states dfa) (transitionMat dfa) ])

--if everything in a row leads to itself its an sink state, return True
isSink :: State -> [State] -> Bool
isSink s = all (== s)