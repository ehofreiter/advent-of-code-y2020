module AOC2020.Day17Part2 where

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

run = runWith "data/day17/input.txt"
test = runWith "data/day17/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let cm = mkCM (readSlice strs)
      cm' = iterate' nextCM cm !! 6
      count = Set.size cm'
  print count

nextCM :: CM -> CM
nextCM cm = Set.fromList $ filter (nextc cm) candidates
  where
    candidates = allCandidates cm

allCandidates :: CM -> [V4 Int]
allCandidates cm = nub $ concatMap getAdj $ Set.toList cm

nextc :: CM -> V4 Int -> Bool
nextc cm v
  =  (active && (activeNbors == 2 || activeNbors == 3))
  || (not active && activeNbors == 3)
  where
    active = cm `atc` v
    activeNbors = length $ filter id $ map (cm `atc`) $ getAdj v

atc :: CM -> V4 Int -> Bool
atc = flip Set.member

getAdj :: V4 Int -> [V4 Int]
getAdj v
  = map (v ^+^)
  $ filter (/= zero)
  $ V4 <$> [-1,0,1] <*> [-1,0,1] <*> [-1,0,1] <*> [-1,0,1]

type CM = Set.Set (V4 Int) -- set of active cubes

mkCM :: [[Bool]] -> CM
mkCM bss = Set.fromList $ do
  (y, bs) <- zip [0..] bss
  (x, b) <- zip [0..] bs
  guard b
  pure (V4 x y 0 0)

showCM :: CM -> String
showCM cm =
  intercalate "\n" $ map showSlice $ expandCM cm
  where
    showSlice (z, bss) = unlines $ ("z=" <> show z) : (map.map) showCube bss
    showCube b = if b then '#' else '.'

expandCM :: CM -> [((Int, Int), [[Bool]])]
expandCM cm = fromMaybe [] $ do
  ws <- enumBy (view _w) cm
  zs <- enumBy (view _z) cm
  ys <- enumBy (view _y) cm
  xs <- enumBy (view _x) cm
  pure [((z, w), mkSlice w z ys xs cm) | z <- zs, w <- ws]
  where
    enumBy :: (V4 Int -> Int) -> CM -> Maybe [Int]
    enumBy f s = do
      let es = Set.map f s
      minE <- Set.lookupMin es
      maxE <- Set.lookupMax es
      pure [minE..maxE]
    mkSlice w z ys xs cm =
      let xyCoords = [[(y, x) | x <- xs] | y <- ys]
      in  (map.map) (\(y, x) -> Set.member (V4 x y z w) cm) xyCoords

readSlice :: [String] -> [[Bool]]
readSlice = (map.map) readChar

readChar :: Char -> Bool
readChar '#' = True
readChar '.' = False
