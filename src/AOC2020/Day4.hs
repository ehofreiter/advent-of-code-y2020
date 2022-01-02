module AOC2020.Day4 where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as P

import AOC2020.Common

run = runWith "data/day4/input.txt"
test = runWith "data/day4/test.txt"
testInvalid = runWith "data/day4/test-invalid.txt"
testValid = runWith "data/day4/test-valid.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let docs = readDocs strs
      pps = map readPassport docs
      c = length . filter id $ map isValid2 pps
  --print docs
  --print pps
  --print (filter (not . isValid2) pps)
  print c

deleteCid :: Passport -> Passport
deleteCid = Map.delete "cid"

isValid :: Passport -> Bool
isValid = (== 7) . Map.size . deleteCid

isValid2 :: Passport -> Bool
isValid2 pp = all ($ pp)
  [ validField "byr" validByr
  , validField "iyr" validIyr
  , validField "eyr" validEyr
  , validField "hgt" validHgt
  , validField "hcl" validHcl
  , validField "ecl" validEcl
  , validField "pid" validPid
  ]

validField :: String -> (String -> Bool) -> Passport -> Bool
validField k valid pp = case Map.lookup k pp of
  Nothing -> False
  Just s -> valid s

validByr :: String -> Bool
validByr s = case P.parse p "" s of
  Left _ -> False
  Right y ->
    let yr = read y :: Int
    in  1920 <= yr && yr <= 2002
  where
    p = P.count 4 P.digit <* P.eof

validIyr :: String -> Bool
validIyr s = case P.parse p "" s of
  Left _ -> False
  Right y ->
    let yr = read y :: Int
    in  2010 <= yr && yr <= 2020
  where
    p = P.count 4 P.digit <* P.eof

validEyr :: String -> Bool
validEyr s = case P.parse p "" s of
  Left _ -> False
  Right y ->
    let yr = read y :: Int
    in  2020 <= yr && yr <= 2030
  where
    p = P.count 4 P.digit <* P.eof

validHgt :: String -> Bool
validHgt s = case P.parse p "" s of
  Left _ -> False
  Right (h, u) ->
    let ht = read h :: Int
    in  case u of
      "cm" -> 150 <= ht && ht <= 193
      "in" -> 59 <= ht && ht <= 76
      _ -> False
  where
    p = (,) <$> P.many P.digit <*> (P.string "cm" <|> P.string "in") <* P.eof

validHcl :: String -> Bool
validHcl s = case P.parse p "" s of
  Left _ -> False
  Right _ -> True
  where
    p = P.char '#' *> P.count 6 P.hexDigit *> P.eof

validEcl :: String -> Bool
validEcl s = case P.parse p "" s of
  Left _ -> False
  Right _ -> True
  where
    p = P.choice (map (P.try . P.string) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
      *> P.eof

validPid :: String -> Bool
validPid s = case P.parse p "" s of
  Left _ -> False
  Right _ -> True
  where
    p = P.count 9 P.digit
      *> P.eof

type Passport = Map.Map String String

readDocs :: [String] -> [[String]]
readDocs = splitOn [""]

readPassport :: [String] -> Passport
readPassport ss = Map.fromList $ map readKvp ws
  where
    ws = concatMap words ss

readKvp :: String -> (String, String)
readKvp s = (k, v)
  where
    k = ss !! 0
    v = ss !! 1
    ss = splitOn ":" s
