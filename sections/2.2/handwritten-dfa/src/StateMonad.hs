module StateMonad where

import Control.Monad.State
import qualified DFA
import Token

type Input = String

data LexerState = LexerState {
    dfaState :: DFA.State    -- current dfa state
  , token    :: Token        -- current parsing token
  , tokens   :: [Token]      -- parsed tokens
  } deriving (Eq, Show)

type LexerStateM = State LexerState

newLexerState :: LexerState
newLexerState = LexerState {
    dfaState = DFA.Start
  , token = newToken
  , tokens = []
  }

getDfaState :: LexerState -> DFA.State
getDfaState = dfaState

finalize :: LexerState -> LexerState
finalize lexerState = finalize' dfaState
  where dfaState = getDfaState lexerState
        finalize' dfaState
          | DFA.isStart dfaState = lexerState
          | DFA.isFinal dfaState = lexerState { tokens = tokens lexerState ++ [token lexerState] }
          | otherwise = error $ "Unexpeted EOF at state " ++ show dfaState

runLexer :: Input -> LexerStateM ()
runLexer "" = do
  lexerState <- get
  put $ finalize lexerState

runLexer input@(char:nextInput) = do
  lexerState <- get
  let currentDfaState = getDfaState lexerState
  let nextDfaState = DFA.dfa currentDfaState char

  when (DFA.isError nextDfaState) $
    fail $ "Unexpected token " ++ show char ++ " near " ++ (show . head . lines) input ++ " at state " ++ show lexerState

  when (DFA.isFinal nextDfaState || DFA.isIntermediate nextDfaState) $ do
    put $ lexerState { dfaState = nextDfaState, token = appendImage (token lexerState) char }
    runLexer nextInput

  when (DFA.isStart nextDfaState) $ do
    when (DFA.isFinal currentDfaState) $
      put $ lexerState { tokens = tokens lexerState ++ [DFA.addTag (token lexerState) currentDfaState] }

    newState <- get
    put $ newState { token = newToken, dfaState = DFA.Start }
    runLexer nextInput

tokenize :: Input -> [Token]
tokenize input = tokens (execState (runLexer input) newLexerState)
