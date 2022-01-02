module AOC2020.Day7 where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as P

import AOC2020.Common

run = runWith "data/day7/input.txt"
test = runWith "data/day7/test.txt"

run2 = runWith2 "data/day7/input.txt"
test2 = runWith2 "data/day7/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let ruleMap = Map.fromList $ map readRule strs
      c = length . filter (flip hasShinyGold ruleMap) . Map.keys $ ruleMap
  print c

runWith2 :: FilePath -> IO ()
runWith2 filePath = do
  strs <- readInputs filePath id
  let ruleMap = Map.fromList $ map readRule strs
      c = countBags "shiny gold" ruleMap
  print c

countBags :: Color -> RuleMap -> Int
countBags c rm = case inners of
  [] -> 0
  ins -> sum $ map f ins
  where
    f (n, ic) = n * (countBags ic rm + 1)
    inners = rm Map.! c

hasShinyGold :: Color -> RuleMap -> Bool
hasShinyGold c rm = any f inners
  where
    f (n, ic) = ic == "shiny gold" || hasShinyGold ic rm
    inners = rm Map.! c

type Color = String
type Inner = (Int, Color)
type Rule = (Color, [Inner])
type RuleMap = Map.Map Color [Inner]
--data RuleTree = RuleTree Color [(Int, RuleTree)]

readRule :: String -> Rule
readRule s = (c, inners)
  where
    [c, rest] = splitOn " bags contain " s
    inners = readInners rest

readInners :: String -> [Inner]
readInners s | s == "no other bags." = []
readInners s = map readInner ss
  where
    ss = splitOn ", " s

readInner :: String -> Inner
readInner s = (read n, unwords cs)
  where
    n:cs = words . head $ endBy " bag" s
