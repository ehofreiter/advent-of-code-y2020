module AOC2020.Day14Part2 where

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

run = runWith "data/day14/input.txt"
test = runWith "data/day14/test2.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let instrs = map readInstr strs
      finalState = foldl' applyInstr ([], Map.empty) instrs
      s = sum $ Map.elems $ snd finalState
  print s

applyInstr :: State -> Instr -> State
applyInstr (mask, memory) instr = case instr of
  Mask mask' -> (mask', memory)
  Mem addr val ->
    let masks = convertMask mask
        addrs = map (flip applyMask addr) masks
        memory' = foldl' (\mem a -> Map.insert a val mem) memory addrs
    in  (mask, memory')

convertMask :: Mask -> [Mask]
convertMask = traverse f
  where
    f (p, True) = [(p, True)]
    f (p, False) = [(p, True), (p, False)]

applyMask :: Mask -> Integer -> Integer
applyMask mask val = foldl' applyBit val mask

applyBit :: Integer -> (Int, Bool) -> Integer
applyBit val (p, True) = setBit val p
applyBit val (p, False) = clearBit val p

type State = (Mask, Map.Map Integer Integer)
type Mask = [(Int, Bool)]

data Instr
  = Mask Mask -- position, bit
  | Mem Integer Integer -- address, value
  deriving (Eq, Show)

readInstr :: String -> Instr
readInstr s =
  if take 2 s == "ma"
  then readMask $ dropWhile (`notElem` "X10") s
  else readMem s

readMask :: String -> Instr
readMask s = Mask i
  where
    i = mapMaybe f $ zip [0..] $ reverse s
    f (pos, c) = case c of
      '0' -> Nothing
      '1' -> Just (pos, True)
      'X' -> Just (pos, False)

readMem :: String -> Instr
readMem s =
  let Right (addr, value) = P.parse memParser "" s
  in  Mem (read addr) (read value)
  where
    memParser = do
      P.string "mem["
      addr <- P.many P.digit
      P.string "] = "
      value <- P.many P.digit
      pure (addr, value)
