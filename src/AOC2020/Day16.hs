module AOC2020.Day16 where

import qualified Linear as L
import Control.Applicative
--import Control.Lens
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
import qualified Text.Parsec as P

import AOC2020.Common

run = runWith "data/day16/input.txt"
test = runWith "data/day16/test.txt"
test2 = runWith "data/day16/test2.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let [fieldStrs, myTicketStr, ticketStrs] = splitOn [""] strs
      fieldMap = map readField fieldStrs
      fields = map snd fieldMap
      myTicket = readTicket $ last myTicketStr :: [Int]
      tickets = map readTicket $ tail ticketStrs
      allValues = concat tickets
      invalidValues = filter (not . validForAnyField fields) allValues
      errorRate = sum invalidValues
      validTickets = filter (not . flip invalidTicket fields) tickets
      validPos = fmap (positionsValidForField validTickets) (Map.fromList fieldMap)
      posMap = stepAll validPos
      departurePoses = Map.elems $ Map.filterWithKey (\n _ -> take 9 n == "departure") posMap
      depVals = map (myTicket !!) departurePoses
      depMul = product depVals
  --print errorRate
  print posMap
  print depMul

stepAll :: Map.Map String [Int] -> Map.Map String Int
stepAll = loop Map.empty
  where
    loop known unknown =
      if Map.null unknown
      then known
      else uncurry loop $ step (known, unknown)

step :: (Map.Map String Int, Map.Map String [Int]) -> (Map.Map String Int, Map.Map String [Int])
step (known, unknown) =
  let known' = fmap head $ Map.filter ((== 1) . length) unknown
      unknown' = foldr Map.delete unknown (Map.keys known')
      unknown'' = foldr (\x m -> fmap (delete x) m) unknown' (Map.elems known')
  in  (known `Map.union` known', unknown'')

positionsValidForField :: [Ticket] -> Field -> [Int]
positionsValidForField ts f =
  filter (\pos -> positionValidForField ts pos f) [0..length (head ts) - 1]

positionValidForField :: [Ticket] -> Int -> Field -> Bool
positionValidForField ts pos f = all (validForField f) $ map (!! pos) ts

invalidTicket :: Ticket -> [Field] -> Bool
invalidTicket t fs = any (not . validForAnyField fs) t

validForAnyField :: [Field] -> Int -> Bool
validForAnyField fs i = any (flip validForField i) fs

validForField :: Field -> Int -> Bool
validForField rs i = any (flip validForRange i) rs

validForRange :: (Int,Int) -> Int -> Bool
validForRange (a,b) i = a <= i && i <= b

type Ticket = [Int]

readTicket :: String -> Ticket
readTicket = map read . splitOn ","

type Field = [(Int, Int)]

readField :: String -> (String, Field)
readField s = (name, map readRange . splitOn " or " $ rest)
  where [name, rest] = splitOn ": " s

readRange :: String -> (Int, Int)
readRange s =
  let [a,b] = splitOn "-" s
  in  (read a,read b)
