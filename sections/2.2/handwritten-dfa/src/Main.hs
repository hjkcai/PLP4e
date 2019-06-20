module Main where

import Data.Char (isSpace, isDigit, isAlpha)

type Input = String
type State = Integer
type Prefix = String
type DFAInput = Char

data TokenizationState = Next State
                       | Whitespace
                       | Error

state :: State -> TokenizationState
state = Next

err :: TokenizationState
err = Error

isFinal :: State -> Bool
isFinal 2  = True
isFinal 6  = True
isFinal 7  = True
isFinal 8  = True
isFinal 9  = True
isFinal 10 = True
isFinal 12 = True
isFinal 14 = True
isFinal 15 = True
isFinal 16 = True
isFinal _  = False

-- DFA for Figure 2.6
dfa :: State -> DFAInput -> TokenizationState

dfa 1 c
  | c == '/'  = state 2
  | c == '('  = state 6
  | c == ')'  = state 7
  | c == '+'  = state 8
  | c == '-'  = state 9
  | c == '*'  = state 10
  | c == ':'  = state 11
  | c == '.'  = state 13
  | isDigit c = state 14
  | isAlpha c = state 16
  | isSpace c = Whitespace
  | otherwise = err

dfa 2 '/' = state 3
dfa 2 '*' = state 4

dfa 3 '\n' = state 1
dfa 3 _    = state 3

dfa 4 '*' = state 5
dfa 4 _   = state 4

dfa 5 '/' = state 1
dfa 5 '*' = state 5
dfa 5 _   = state 4

dfa 11 '=' = state 12

dfa 13 c
  | isDigit c = state 15
  | isSpace c = Whitespace
  | otherwise = err

dfa 14 c
  | c == '.'  = state 15
  | isDigit c = state 14
  | isSpace c = Whitespace
  | otherwise = err

dfa 15 c
  | isDigit c = state 15
  | isSpace c = Whitespace
  | otherwise = err

dfa 16 c
  | isDigit c = state 16
  | isAlpha c = state 16
  | isSpace c = Whitespace
  | otherwise = err

dfa _ ' ' = Whitespace
dfa _ _   = err

tokenizeInternal :: Input -> State -> Prefix -> [String]
tokenizeInternal "" state prefix
  | state == 0    = []
  | state == 1    = [prefix]
  | isFinal state = [prefix]
  | otherwise     = error $ "Unexpeted EOF at state " <> show state
tokenizeInternal input@(x:xs) state prefix = finalize $ dfa state x
  where finalize (Next nextState) = tokenizeInternal xs nextState (prefix <> [x])
        finalize Error = error $ "Unexpected token '" <> [x] <> "' at state " <> show state <> " near " <> input
        finalize Whitespace
          | isFinal state = prefix : tokenizeInternal xs 1 ""
          | otherwise     = finalize Error

tokenize :: Input -> [String]
tokenize input = filter (not . null) $ tokenizeInternal input 1 ""

main :: IO ()
main = (tokenize <$> readFile "input.txt") >>= print
