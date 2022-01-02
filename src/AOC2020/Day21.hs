module AOC2020.Day21 where

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
import Linear hiding (transpose)
import qualified Text.Parsec as P

import AOC2020.Common

import AOC2020.CoordVec as CV

runF =  "data/day21/input.txt"
testF = "data/day21/test.txt"

run = runWith runF
test = runWith testF

load :: FilePath -> IO [Food]
load filePath = do
  strs <- readInputs filePath id
  pure (map readFood strs)

runWith :: FilePath -> IO ()
runWith filePath = do
  foods <- load filePath
  print (part1 foods)
  putStrLn (part2 foods)

part2 :: [Food] -> String
part2 foods =
  let aim = mkAim foods
      kaim = solve aim
      ingrs = Map.elems kaim
  in  intercalate "," ingrs

solve :: AIM -> KAIM
solve = loop Map.empty
  where
    loop kaim aim
      | Map.null aim = kaim
      | otherwise =
        let (kaim', aim') = extractKnown aim
        in  loop (Map.union kaim kaim') (removeKnown aim' kaim')

type KAIM = Map.Map Allergen Ingr

removeKnown :: AIM -> KAIM -> AIM
removeKnown aim kaim =
  let ingrs = Set.fromList $ Map.elems kaim
  in  Map.map (Set.\\ ingrs) aim

extractKnown :: AIM -> (KAIM, AIM)
extractKnown aim =
  let (known, rest) = Map.partition ((== 1) . Set.size) aim
  in  (Map.map (head . Set.toList) known, rest)

part1 :: [Food] -> Int
part1 foods =
  let aim = mkAim foods
      allergenic = allergenicIngrs aim
      allI = allIngrs foods
      nonAllergenic = allI Set.\\ allergenic
      ingrF = ingrFreqs foods
  in  sum $ Map.restrictKeys ingrF nonAllergenic

ingrFreqs :: [Food] -> Map.Map Ingr Int
ingrFreqs = mkHistogram . foldl' (++) [] . map fst

allergenicIngrs :: AIM -> Set.Set Ingr
allergenicIngrs = Map.foldl' Set.union Set.empty

allIngrs :: [Food] -> Set.Set Ingr
allIngrs = foldl' Set.union Set.empty . map (Set.fromList . fst)

mkAim :: [Food] -> AIM
mkAim = foldl' aimAddFood Map.empty

type IAM = Map.Map Ingr (Set.Set Allergen)
type AIM = Map.Map Allergen (Set.Set Ingr)

aimAddFood :: AIM -> Food -> AIM
aimAddFood aim (ingrs, allergens) =
  foldl' (\m a -> aimAddAllergen m a ingrSet) aim allergens
  where
    ingrSet = Set.fromList ingrs

aimAddAllergen :: AIM -> Allergen -> Set.Set Ingr -> AIM
aimAddAllergen aim allergen ingrs =
  Map.insertWith Set.intersection allergen ingrs aim

type Ingr = String
type Allergen = String

type Food = ([Ingr], [Allergen])

readFood :: String -> ([Ingr], [Allergen])
readFood s =
  let [ingrStr, allergenStr] = splitOn " (contains " s
      ingrs = words ingrStr
      allergens = splitOn ", " $ init allergenStr
  in  (ingrs, allergens)
