module AOC2020.Day23 where

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

runF =  "871369452"
testF = "389125467"

run = runWith runF
test = runWith testF

runWith :: String -> IO ()
runWith input = do
  let cups = readCups input
  print (part1 cups)

part1 :: Cups -> String
part1 cups =
  let finalCups = (!! 100) $ iterate' move cups
  in  concatMap show $ toList $ afterCup1 finalCups

afterCup1 :: Cups -> Cups
afterCup1 cups =
  let (before, one :<| after) = Seq.breakl (== 1) cups
  in  after >< before

move :: Cups -> Cups
move (current :<| nonCurrent) =
  let (theThree, theRest) = Seq.splitAt 3 nonCurrent
      dest = destLabel current theRest
      (before, d :<| after) = Seq.breakl (== dest) theRest
  in  ((before :|> d) >< theThree >< after) :|> current

destLabel :: Int -> Cups -> Int
destLabel current theRest =
  case Seq.filter (< current) theRest of
    Seq.Empty -> maximum theRest
    lessThan -> maximum lessThan
type Cups = Seq Int

readCups :: String -> Seq Int
readCups = Seq.fromList . map (read . (:[]))
