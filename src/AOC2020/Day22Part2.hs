module AOC2020.Day22Part2 where

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
  print (part2 (deck1, deck2))

part2 :: Decks -> Int
part2 = score . snd . play

score :: Deck -> Int
score deck = sum $ zipWith (*) [1..] $ toList $ Seq.reverse deck

play :: Decks -> Result
play decks = loop (decks, Set.empty)
  where
    loop state = case playRound state of
      Left result -> result
      Right state' -> loop state'

viewMany :: Decks -> [Decks]
viewMany = mapMaybe f . playMany
  where
    f (Left r) = Nothing
    f (Right (ds, prev)) = Just ds

playMany :: Decks -> [Either Result State]
playMany decks = iterateM playRound $ pure (decks, Set.empty)

type Decks = (Deck, Deck) -- p1, p2
type State = (Decks, Set.Set Decks) -- current decks, all previous states
type Result = (Bool, Deck) -- isPlayer1 winner, winning deck

playRound :: State -> Either Result State
playRound ((Seq.Empty, winner), _) = Left (False, winner)
playRound ((winner, Seq.Empty), _) = Left (True, winner)
playRound (decks@(c1 :<| d1, c2 :<| d2), prevDecks)
  | Set.member decks prevDecks = Left (True, c1 :<| d1)
  | otherwise =
    let isWinnerP1 = fromMaybe (c1 > c2) $ tryRecurseRound (c1, c2) (d1, d2)
        decks' =
          if isWinnerP1
          then (d1 :|> c1 :|> c2, d2)
          else (d1, d2 :|> c2 :|> c1)
    in  Right (decks', Set.insert decks prevDecks)

tryRecurseRound :: (Int, Int) -> Decks -> Maybe Bool
tryRecurseRound (c1, c2) (d1, d2) =
  let d1' = Seq.take c1 d1
      d2' = Seq.take c2 d2
  in  if Seq.length d1' == c1 && Seq.length d2' == c2
      then Just $ fst $ play (d1', d2')
      else Nothing

type Deck = Seq.Seq Int

readDecks :: [String] -> (Deck, Deck)
readDecks ss =
  let [p1, p2] = splitOn [""] ss
      deck1 = Seq.fromList $ map read $ tail p1
      deck2 = Seq.fromList $ map read $ tail p2
  in  (deck1, deck2)
