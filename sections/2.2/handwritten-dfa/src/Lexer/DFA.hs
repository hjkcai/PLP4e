module Lexer.DFA where

import Lexer.Token

data DFA = DFA { transition :: DFAState -> Input -> DFAState    -- |The DFA transition function
               , tagName :: DFAState -> Token -> String         -- |The tag name generator
               }

type Input = Char
type Prefix = String

data DFAState = Start
                | Intermediate Integer
                | Final Integer
                | Error
                  deriving (Eq, Show)

isStart :: DFAState -> Bool
isStart Start = True
isStart _     = False

isIntermediate :: DFAState -> Bool
isIntermediate (Intermediate _) = True
isIntermediate _                = False

isFinal :: DFAState -> Bool
isFinal (Final _) = True
isFinal _         = False

isError :: DFAState -> Bool
isError Error = True
isError _     = False
