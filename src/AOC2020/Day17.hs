module AOC2020.Day17 where

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

allCandidates :: CM -> [V3 Int]
allCandidates cm = nub $ concatMap getAdj $ Set.toList cm

nextc :: CM -> V3 Int -> Bool
nextc cm v
  =  (active && (activeNbors == 2 || activeNbors == 3))
  || (not active && activeNbors == 3)
  where
    active = cm `atc` v
    activeNbors = length $ filter id $ map (cm `atc`) $ getAdj v

atc :: CM -> V3 Int -> Bool
atc = flip Set.member

getAdj :: V3 Int -> [V3 Int]
getAdj v
  = map (v ^+^)
  $ filter (/= zero)
  $ V3 <$> [-1,0,1] <*> [-1,0,1] <*> [-1,0,1]

type CM = Set.Set (V3 Int) -- set of active cubes

mkCM :: [[Bool]] -> CM
mkCM bss = Set.fromList $ do
  (y, bs) <- zip [0..] bss
  (x, b) <- zip [0..] bs
  guard b
  pure (V3 x y 0)

showCM :: CM -> String
showCM cm =
  intercalate "\n" $ map showSlice $ expandCM cm
  where
    showSlice (z, bss) = unlines $ ("z=" <> show z) : (map.map) showCube bss
    showCube b = if b then '#' else '.'

expandCM :: CM -> [(Int, [[Bool]])]
expandCM cm = fromMaybe [] $ do
  zs <- enumBy (view _z) cm
  ys <- enumBy (view _y) cm
  xs <- enumBy (view _x) cm
  pure [(z, mkSlice z ys xs cm) | z <- zs]
  where
    enumBy :: (V3 Int -> Int) -> CM -> Maybe [Int]
    enumBy f s = do
      let es = Set.map f s
      minE <- Set.lookupMin es
      maxE <- Set.lookupMax es
      pure [minE..maxE]
    mkSlice z ys xs cm =
      let xyCoords = [[(y, x) | x <- xs] | y <- ys]
      in  (map.map) (\(y, x) -> Set.member (V3 x y z) cm) xyCoords

readSlice :: [String] -> [[Bool]]
readSlice = (map.map) readChar

readChar :: Char -> Bool
readChar '#' = True
readChar '.' = False
