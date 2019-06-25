module DFA where

import Data.Char (isSpace, isDigit, isAlpha)
import Token

type Input = Char
type Prefix = String

data State = Start
           | Intermediate Integer
           | Final Integer
           | Error
             deriving (Eq, Show)

isStart :: State -> Bool
isStart Start = True
isStart _     = False

isIntermediate :: State -> Bool
isIntermediate (Intermediate _) = True
isIntermediate _                = False

isFinal :: State -> Bool
isFinal (Final _) = True
isFinal _         = False

isError :: State -> Bool
isError Error = True
isError _     = False

getTag :: State -> Token -> String
getTag (Final 2) _  = "div"
getTag (Final 6) _  = "lparen"
getTag (Final 7) _  = "rparen"
getTag (Final 8) _  = "plus"
getTag (Final 9) _  = "minus"
getTag (Final 10) _ = "times"
getTag (Final 12) _ = "assign"
getTag (Final 14) _ = "number"
getTag (Final 15) _ = "number"
getTag (Final 16) token
  | image token == "read" || image token == "write" = "keyword"
  | otherwise = "id"

addTag :: Token -> State -> Token
addTag token state = token { tag = getTag state token }

dfa :: State -> Input -> State
dfa state c = case state of
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