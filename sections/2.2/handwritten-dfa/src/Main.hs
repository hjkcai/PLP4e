module Main where

import StateMonad

main :: IO ()
main = tokenize <$> readFile "input.txt" >>= mapM_ print
