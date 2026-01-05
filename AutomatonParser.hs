module AutomatonParser
  ( DFA(..)
  , parseDFAFromFile
  , State
  ) where

import System.IO
import System.Environment

--Makes things readable
type State = String
type Symbol = String

--Might need to change the transitionMat in future to look more like a monoid, it should make aperiodicity test way easier

data DFA = DFA
  { states        :: [State]
  , alphabet      :: [Symbol]
  , startState    :: State
  , acceptStates  :: [State]
  , transitions   :: [((State, Symbol), State)]
  , transitionMat :: [[State]]   -- 2D matrix: states x alphabet
  } deriving Show

isSpaceChar :: Char -> Bool
isSpaceChar c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

trim :: String -> String
trim = dropWhile isSpaceChar . reverse . dropWhile isSpaceChar . reverse

isPrefixOf' :: String -> String -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

parseKeyValues :: String -> [String]
parseKeyValues =
  words . trim . drop 1 . dropWhile (/= ':')

parseTransition :: String -> ((State, Symbol), State)
parseTransition line =
  case words line of
    [from, sym, to] -> ((from, sym), to)
    _ -> error ("Invalid transition line: " ++ line)

-- We assume full DFA with deterministic transitions to make parsing easier
lookupTransition :: State -> Symbol -> [((State, Symbol), State)] -> State
lookupTransition s a ts =
  case [t | ((s', a'), t) <- ts, s == s', a == a'] of
    [x] -> x
    []  -> error ("Missing transition: " ++ s ++ " " ++ a)
    _   -> error ("Non-deterministic transitions at: " ++ s ++ " " ++ a)

buildMatrix :: [State] -> [Symbol] -> [((State, Symbol), State)] -> [[State]]
buildMatrix sts syms ts =
  [ [ lookupTransition s a ts | a <- syms ] | s <- sts ]

parseDFA :: String -> DFA
parseDFA content =
  let ls = lines content

      statesLine =
        head [l | l <- ls, "states:" `isPrefixOf'` l]

      alphabetLine =
        head [l | l <- ls, "alphabet:" `isPrefixOf'` l]

      startLine =
        head [l | l <- ls, "start:" `isPrefixOf'` l]

      acceptingLine =
        head [l | l <- ls, "accepting:" `isPrefixOf'` l]

      transitionLines =
        filter (not . null)
        (drop 1 (dropWhile (/= "transitions:") ls))

      sts = parseKeyValues statesLine
      syms = parseKeyValues alphabetLine
      ts = map parseTransition transitionLines

  in DFA
      { states        = sts
      , alphabet      = syms
      , startState    = head (parseKeyValues startLine)
      , acceptStates  = parseKeyValues acceptingLine
      , transitions   = ts
      , transitionMat = buildMatrix sts syms ts
      }

parseDFAFromFile :: FilePath -> IO DFA
parseDFAFromFile path = do
  content <- readFile path
  return (parseDFA content)
