module AOC2020.Day23Part2MV where

import Data.List
import qualified Data.Vector.Mutable as MV

import AOC2020.Common

runF =  "871369452"
testF = "389125467"

runWith :: String -> IO ()
runWith input = do
  let ints = readInitialCups input
  s0 <- initialState (10^6) ints
  part2 <- moveN (10^7) s0
  print part2

moveN :: Int -> State -> IO Int
moveN n (current, cups) = loop n current
  where
    loop :: Int -> Int -> IO Int
    loop n current
      | n <= 0 = do
          a <- MV.read cups 0
          b <- MV.read cups a
          pure $ (a+1) * (b+1)
      | otherwise = do
          a <- MV.read cups current
          b <- MV.read cups a
          c <- MV.read cups b
          current' <- MV.read cups c
          let dest = prevLabel current (a,b,c) (MV.length cups)
          dest1 <- MV.read cups dest
          MV.write cups current current'
          MV.write cups dest a
          MV.write cups c dest1
          loop (n-1) current'

prevLabel :: Int -> (Int, Int, Int) -> Int -> Int
prevLabel n (a, b, c) cupCount =
  head $ drop 1 $ iterate prevInt n \\ [a,b,c]
  where
    prevInt 0 = cupCount - 1
    prevInt n = n - 1

type State = (Int, Cups)
type Cups = MV.IOVector Int

initialState :: Int -> [Int] -> IO State
initialState n ints = do
  cups <- mkCups n ints
  pure (head ints, cups)

mkCups :: Int -> [Int] -> IO Cups
mkCups n ints = MV.generate n f
  where
    start = length ints
    f i | i == n - 1 = head ints
        | i >= start = i + 1
        | otherwise  = case dropWhile (/= i) ints of
            [] -> error "halp"
            [_] -> start
            _:n:_ -> n

readInitialCups :: String -> [Int]
readInitialCups = map (pred . read . (:[]))

