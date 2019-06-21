module Main where

import InitialState

main :: IO ()
main = tokenize <$> readFile "input.txt" >>= mapM_ print
