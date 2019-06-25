module Main where

import Data.Char (isSpace, isDigit, isAlpha)
import Lexer hiding (Input)
import Lexer.DFA
import Lexer.Token

main :: IO ()
main = do
  tokens <- tokenize dfa <$> readFile "input.txt"
  mapM_ print tokens

dfa :: DFA
dfa = DFA { transition = trans, tagName = tag } where
  tag (Final 2) _  = "div"
  tag (Final 6) _  = "lparen"
  tag (Final 7) _  = "rparen"
  tag (Final 8) _  = "plus"
  tag (Final 9) _  = "minus"
  tag (Final 10) _ = "times"
  tag (Final 12) _ = "assign"
  tag (Final 14) _ = "number"
  tag (Final 15) _ = "number"
  tag (Final 16) token
    | image token == "read" || image token == "write" = "keyword"
    | otherwise = "id"

  trans state c = case state of
    Start -> next c
      where next '/' = Final 2
            next '(' = Final 6
            next ')' = Final 7
            next '+' = Final 8
            next '-' = Final 9
            next '*' = Final 10
            next ':' = Intermediate 11
            next '.' = Intermediate 13
            next c
              | isDigit c = Final 14
              | isAlpha c = Final 16
              | isSpace c = Start
              | otherwise = Error

    Final 2 -> next c
      where next '/' = Intermediate 3
            next '*' = Intermediate 4
            next c
              | isSpace c = Start
              | otherwise = Error

    Intermediate 3 -> next c
      where next '\n' = Start
            next _    = Intermediate 3

    Intermediate 4 -> next c
      where next '*' = Intermediate 5
            next _   = Intermediate 4

    Intermediate 5 -> next c
      where next '/' = Start
            next '*' = Intermediate 5
            next _   = Intermediate 4

    Intermediate 11 -> next c
      where next '=' = Final 12
            next _   = Error

    Intermediate 13 -> next c
      where next c
              | isDigit c = Final 15
              | isSpace c = Start
              | otherwise = Error

    Final 14 -> next c
      where next '.' = Final 15
            next c
              | isDigit c = Final 14
              | isSpace c = Start
              | otherwise = Error

    Final 15 -> next c
      where next c
              | isDigit c = Final 15
              | isSpace c = Start
              | otherwise = Error

    Final 16 -> next c
      where next c
              | isDigit c = Final 16
              | isAlpha c = Final 16
              | isSpace c = Start
              | otherwise = Error

    _ -> next c
      where next c
              | isSpace c = Start
              | otherwise = Error
