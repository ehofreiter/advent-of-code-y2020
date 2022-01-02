module AOC2020.Day22 where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Functor.WithIndex
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><))
import Linear hiding (transpose)
import qualified Text.Parsec as P

import AOC2020.Common

runF =  "data/day22/input.txt"
testF = "data/day22/test.txt"

run = runWith runF
test = runWith testF

load :: FilePath -> IO (Deck, Deck)
load filePath = do
  strs <- readInputs filePath id
  pure (readDecks strs)

runWith :: FilePath -> IO ()
runWith filePath = do
  (deck1, deck2) <- load filePath
  print deck1
  print deck2
  print (part1 (deck1, deck2))

part1 :: (Deck, Deck) -> Int
part1 = score . play

score :: Deck -> Int
score deck = sum $ zipWith (*) [1..] $ toList $ Seq.reverse deck

play :: (Deck, Deck) -> Deck
play decks = case playRound decks of
  Left winner -> winner
  Right decks' -> play decks'

playMany :: (Deck, Deck) -> [Either Deck (Deck, Deck)]
playMany = iterateM playRound . pure

playRound :: (Deck, Deck) -> Either Deck (Deck, Deck)
playRound (Seq.Empty, winner) = Left winner
playRound (winner, Seq.Empty) = Left winner
playRound (c1 :<| d1, c2 :<| d2)
  | c1 > c2 = Right (d1 :|> c1 :|> c2, d2)
  | c1 < c2 = Right (d1, d2 :|> c2 :|> c1)
  | otherwise = error "tie!"

type Deck = Seq.Seq Int

readDecks :: [String] -> (Deck, Deck)
readDecks ss =
  let [p1, p2] = splitOn [""] ss
      deck1 = Seq.fromList $ map read $ tail p1
      deck2 = Seq.fromList $ map read $ tail p2
  in  (deck1, deck2)
