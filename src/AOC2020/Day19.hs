module AOC2020.Day19 where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.Foldable
import Data.Functor.WithIndex
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Linear
import qualified Text.Parsec as P

import AOC2020.Common

runF =  "data/day19/input.txt"
testF = "data/day19/test.txt"
test2F = "data/day19/test2.txt"

run = runWith runF
test = runWith testF
test2 = runWith test2F

load :: FilePath -> IO (RM, [String])
load filePath = do
  strs <- readInputs filePath id
  let [ruleStrs, msgs] = splitOn [""] strs
      rules = map readRule ruleStrs
      rm = Map.fromList rules
  pure (rm, msgs)

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let [ruleStrs, msgs] = splitOn [""] strs
      rules = map readRule ruleStrs
      rm = Map.fromList rules
      parser0 = parser rm (rm Map.! 0)
      results = map (\m -> (m, match parser0 m)) msgs
      part1 = length $ filter snd results
      rule8 = Or (Then [42]) (Then [42,8])
      rule11 = Or (Then [42,31]) (Then [42,11,31])
      rm2 = Map.insert 0 Rule0 $ Map.insert 11 Rule11 $ Map.insert 8 Rule8 rm
      parser02 = parser rm2 (rm2 Map.! 0)
      results2 = map (\m -> (m, match parser02 m)) msgs
      part2 = length $ filter snd results2
  --mapM_ print rules
  --mapM_ print msgs
  print part1
  print part2

parse :: P.Parsec String () () -> String -> Either P.ParseError ()
parse p s = P.parse (p <* P.eof) "parse" s

match :: P.Parsec String () () -> String -> Bool
match p s =
  case P.parse (p <* P.eof) "match" s of
    Left err -> False
    Right _ -> True

parser :: RM -> Rule -> P.Parsec String () ()
parser rm r = flip P.label (show r) $ case r of
  C c -> void $ P.char c
  Then ris ->
    traverse_ (parser rm . (rm Map.!)) ris
  Or r0 r1 ->
    void $ P.try (parser rm r0) <|> P.try (parser rm r1)
  Rule0 -> do
    n <- P.many1 (parser rm (rm Map.! 42))
    m <- P.many1 (parser rm (rm Map.! 31))
    if length n < 2 || length m > length n - 1
      then fail "too many 31s"
      else pure ()
  Rule8 ->
    void $ fail "Hit Rule 8!"
  Rule11 ->
    void $ fail "Hit Rule 11!"

type RM = Map.Map Int Rule
data Rule
  = C Char
  | Then [Int]
  | Or Rule Rule
  | Rule0
  | Rule8
  | Rule11
  deriving (Eq, Ord, Show)

flatten :: RM -> Map.Map Int RuleA
flatten rm = Map.map f rm
  where
    f :: Rule -> RuleA
    f r = case r of
      C c -> CA c
      Then ids -> ThenA $ map (f . (rm Map.!)) ids
      Or r0 r1 -> OrA (f r0) (f r1)

makeMatches :: RuleA -> [String]
makeMatches ra = case ra of
  CA c -> [[c]]
  ThenA rs -> concat <$> traverse makeMatches rs
  OrA r0 r1 -> makeMatches r0 <> makeMatches r1

data RuleA
  = CA Char
  | ThenA [RuleA]
  | OrA RuleA RuleA
  deriving (Eq, Ord)

instance Show RuleA where
  show ra = case ra of
    CA c -> [c]
    ThenA ras -> concatMap show ras
    OrA r0 r1 -> "(" <> show r0 <> "|" <> show r1 <> ")"

showRule = showRuleAt 0

showRuleAt lvl ra = case ra of
  CA c -> [c]
  ThenA ras -> concatMap (showRuleAt lvl) ras
  OrA r0 r1 -> "(" <> l <> showRuleAt (lvl + 1) r0
            <> "|" <> l <> showRuleAt (lvl + 1) r1
            <> ")" <> l
  where
    l = show lvl

readRule :: String -> (Int, Rule)
readRule s =
  let [i, r] = splitOn ": " s
  in  case r of
    ['"',c,'"'] -> (read i, C c)
    _ ->
      let ors = splitOn " | " r
          thens = map (Then . map read . splitOn " ") ors
      in  case thens of
            [t] -> (read i, t)
            [t0, t1] -> (read i, Or t0 t1)
