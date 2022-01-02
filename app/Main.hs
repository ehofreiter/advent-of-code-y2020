module Main where

import System.Environment

import AOC2020.Day24

main :: IO ()
main = do
  args <- getArgs
  runWith (head args)
