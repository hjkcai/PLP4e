module Lexer.Token where

data Token = Token { image :: String
                   , tag :: String
                   } deriving (Eq, Show)

newToken :: Token
newToken = Token { image = "", tag = "" }

appendImage :: Token -> Char -> Token
appendImage token char = token { image = image token ++ [char] }
