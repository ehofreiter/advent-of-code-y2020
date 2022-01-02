module AOC2020.Day8 where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P

import AOC2020.Common

run = runWith "data/day8/input.txt"
test = runWith "data/day8/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let instrs = map readInstr strs
      prog = mkProgram instrs
      progs = mkPrograms prog
      results = map execute progs
      mState = find (not . fst) results
    in
    case mState of
      Nothing -> error "nothing found"
      Just state@(looped, (acc, pc, pcs)) -> do
        print state

execute :: Program -> (Bool, State)
execute = findLoop (0, 0, Set.empty)

mkProgram :: [Instr] -> Program
mkProgram = Map.fromList . zip [0..]

data Instr
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving (Eq, Ord, Show)

type State = (Int, Int, Set.Set Int) -- accum, pc, prev pcs

type Program = Map.Map Int Instr

-- Change one nop to jmp or jmp to nop
mkPrograms :: Program -> [Program]
mkPrograms prog =
  let nopToJmps = mapMaybe (traverse nopToJmp) $ Map.toList prog
      jmpToNops = mapMaybe (traverse jmpToNop) $ Map.toList prog
      ntjProgs = map (\(i, instr) -> Map.insert i instr prog) nopToJmps
      jtnProgs = map (\(i, instr) -> Map.insert i instr prog) jmpToNops
  in  jtnProgs ++ ntjProgs

nopToJmp :: Instr -> Maybe Instr
nopToJmp (Nop n) = Just (Jmp n)
nopToJmp _ = Nothing

jmpToNop :: Instr -> Maybe Instr
jmpToNop (Jmp n) = Just (Nop n)
jmpToNop _ = Nothing

isNop :: Instr -> Bool
isNop (Nop _) = True
isNop _ = False

isJmp :: Instr -> Bool
isJmp (Jmp _) = True
isJmp _ = False

findLoop :: State -> Program -> (Bool, State)
findLoop state@(acc, pc, pcs) prog =
  if Set.member pc pcs
     then (True, state)
     else case mInstr of
            Nothing -> (False, state)
            Just instr -> findLoop (step state instr) prog
  where
    mInstr = Map.lookup pc prog

step :: State -> Instr -> State
step (acc, pc, pcs) instr = case instr of
  Acc n -> (acc + n, pc + 1, pcs')
  Jmp n -> (acc, pc + n, pcs')
  Nop n -> (acc, pc + 1, pcs')
  where
    pcs' = Set.insert pc pcs

readInstr :: String -> Instr
readInstr s =
  let [si, sn] = words s
      n = read (stripPlus sn)
  in  case si of
    "acc" -> Acc n
    "jmp" -> Jmp n
    "nop" -> Nop n

stripPlus :: String -> String
stripPlus ('+':s) = s
stripPlus s = s
