{-# LANGUAGE MultiWayIf #-}

module Lexer.Lexer where

import Control.Monad.State
import Lexer.DFA hiding (Input)
import Lexer.Token

type Input = String

data LexerState = LexerState
  { dfaState :: DFAState     -- |current dfa state
  , token    :: Token        -- |current parsing token
  , tokens   :: [Token]      -- |parsed tokens
  } deriving (Eq, Show)

type LexerStateM = Control.Monad.State.State LexerState

-- |For easier pattern matching
type LexerStateTuple = (LexerState, DFAState, Token, [Token])

newLexerState :: LexerState
newLexerState = LexerState
  { dfaState = Start
  , token = newToken
  , tokens = []
  }

nextLexerState :: DFAState -> Token -> LexerStateM ()
nextLexerState dfaState token = modify (\lexerState -> lexerState { dfaState = dfaState, token = token })

getState :: LexerStateM LexerStateTuple
getState = do
  lexerState@LexerState{ dfaState = dfaState, token = token, tokens = tokens } <- get
  return (lexerState, dfaState, token, tokens)

getTaggedToken :: DFA -> LexerStateM Token
getTaggedToken DFA{ tagName = tn } = gets taggedToken
  where taggedToken LexerState{ token = t, dfaState = s } = t { tag = tn s t }

pushToken :: DFA -> LexerStateM ()
pushToken dfa = do
  (lexerState, dfaState, _, tokens) <- getState
  when (isFinal dfaState) $ do
    taggedToken <- getTaggedToken dfa
    put $ lexerState { tokens = tokens ++ [taggedToken] }

unexpectedToken :: Input -> LexerStateM ()
unexpectedToken input@(char:_) = do
  lexerState <- get
  fail $ "Unexpected token " ++ show char ++ " near " ++ (show . head . lines) input ++ " at state " ++ show lexerState

runLexer :: DFA -> Input -> LexerStateM ()
runLexer _ "" = getState >>= put . finalize   -- EOF handler
  where finalize (lexerState, dfaState, token, tokens)
          | isStart dfaState = lexerState
          | isFinal dfaState = lexerState { tokens = tokens ++ [token] }
          | otherwise = error $ "Unexpeted EOF at state " ++ show dfaState

runLexer dfa input@(char:nextInput) = do      -- non-EOF handler
  (lexerState, dfaState, token, tokens) <- getState
  let nextDfaState = transition dfa dfaState char

  if  | isError nextDfaState -> unexpectedToken input
      | isFinal nextDfaState || isIntermediate nextDfaState -> do
          nextLexerState nextDfaState (appendImage token char)
          runLexer dfa nextInput
      | isStart nextDfaState -> do
          pushToken dfa
          nextLexerState Start newToken
          runLexer dfa nextInput

tokenize :: DFA -> Input -> [Token]
tokenize dfa input = tokens (execState (runLexer dfa input) newLexerState)
