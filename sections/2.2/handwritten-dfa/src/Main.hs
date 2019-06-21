module Main where

import BetterState

main :: IO ()
main = tokenize <$> readFile "input.txt" >>= mapM_ print
