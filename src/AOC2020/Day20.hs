module AOC2020.Day20 where

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

runF =  "data/day20/input.txt"
testF = "data/day20/test.txt"

run = runWith runF
test = runWith testF

load :: FilePath -> IO TM
load filePath = do
  strs <- readInputs filePath id
  pure (readTM strs)

runWith :: FilePath -> IO ()
runWith filePath = do
  tm <- load filePath
  let ms = findAllMatches tm
      mcs = matchCounts ms
      corners = Map.keys $ Map.filter (== 2) mcs
      part1 = product corners
      ptm = placeTiles $ refineMatchMap $ findAllMatches tm
      bss = glueTiles $ removeAllEdges $ assembleTiles ptm tm
      bigTile = CV.fromLists bss
  -- print mcs
  -- print corners
  -- print part1
  mapM_ putStrLn $ (map.map) showSpot bss
  print $ part2 bigTile

part2 :: Tile -> Int
part2 tile =
  let tileOrients = allOrientations tile
      monsterCount = maximum $ map countSeaMonsters tileOrients
      totalHashCount = length $ filter id $ concat $ CV.toLists tile
      monsterHashCount = monsterCount * length seaMonsterCoords
  in  totalHashCount - monsterHashCount

countSeaMonsters :: Tile -> Int
countSeaMonsters tile =
  length $ filter id [isSeaMonsterAt tile (x,y) | x <- xs, y <- ys]
  where
    xs = [0..rowSize tile - 1]
    ys = [0..colSize tile - 1]

isSeaMonsterAt :: Tile -> (Int, Int) -> Bool
isSeaMonsterAt tile (x,y) =
  let shiftedSm = map (addPos (x,y)) seaMonsterCoords
  in  all (== Just True) $ map (tile CV.!?) shiftedSm

seaMonsterCoords :: [(Int, Int)]
seaMonsterCoords = [(x,y) | x <- xs, y <- ys, smcv CV.! (x,y) == '#']
  where
    xs = [0..rowSize smcv - 1]
    ys = [0..colSize smcv - 1]
    smcv = CV.fromLists
      ["                  # "
      ,"#    ##    ##    ###"
      ," #  #  #  #  #  #   "
      ]

assembleTiles :: PTM -> TM -> [[[[Bool]]]]
assembleTiles ptm tm =
  let placedTileRows = transpose $ groupBy sameX $ sortBy cmpPos $ Map.assocs ptm
  in  (map.map) (getTransformedTile tm) placedTileRows

removeAllEdges :: [[[[a]]]] -> [[[[a]]]]
removeAllEdges = (map.map) removeTileEdges

removeTileEdges :: [[a]] -> [[a]]
removeTileEdges = map (init . tail) . init . tail

glueTiles :: [[[[a]]]] -> [[a]]
glueTiles = concatMap (foldl' appendRows (repeat []))

getPos (_, (_, _, pos)) = pos
cmpPos a b = compare (getPos a) (getPos b)
getX (_, (_, _, (x, _))) = x
sameX a b = getX a == getX b

appendRows :: [[a]] -> [[a]] -> [[a]]
appendRows = zipWith (++)

allOrientations :: Tile -> [Tile]
allOrientations tile =
  map (CV.fromLists . (\(f, r) -> transformTile f r tileLists)) flipRots
  where
    tileLists = CV.toLists tile
    flipRots = (,) <$> [False,True] <*> [0..3]

transformTile :: Bool -> Int -> [[Bool]] -> [[Bool]]
transformTile isFlipped rot tile =
  let tile' = if isFlipped then map reverse tile else tile
  in  iterate' rotLeft tile' !! rot
  where
    rotLeft = reverse . transpose

getTransformedTile :: TM -> (Int, (Bool, Int, (Int, Int))) -> [[Bool]]
getTransformedTile tm (tid, (isFlipped, rot, _)) =
  let tileLists = CV.toLists (tm Map.! tid)
  in  transformTile isFlipped rot tileLists

-- flipped, quarter-turns rotated left, position
type PlacedTile = (Bool, Int, (Int, Int))
type PTM = Map.Map Int PlacedTile -- tile id, placed tile

placeFirstTile :: RMM -> PTM
placeFirstTile rmm =
  let tid = fst $ head $ Map.keys rmm
      firstPlacedTile = (False, 0, (0,0))
  in  Map.singleton tid firstPlacedTile

placeTile :: RMM -> PTM -> Maybe PTM
placeTile rmm ptm
  | Map.null ptm = Just $ placeFirstTile rmm
  | otherwise =
    case Map.lookupMin $ placedEdges rmm ptm of
      Nothing -> Nothing
      Just ((tid, eid), (isFlippedRelative', (tid', eid'))) ->
        let (isFlipped, rot, pos) = ptm Map.! tid
            transEid = transformEdgeId isFlipped rot eid
            -- based on whether the placed tile is flipped and whether the next
            -- tile is flipped relative to the placed tile, determine whether
            -- the next tile is flipped (in absolute terms)
            isFlipped' = isFlipped /= isFlippedRelative'
            rot' = getRotTo isFlipped' (oppositeEdgeId transEid) eid'
            pos' = pos `addPos` dirFromEid transEid
            ptile' = (isFlipped', rot', pos')
            ptm' = Map.insert tid' ptile' ptm
        in  Just ptm'

placeTiles :: RMM -> PTM
placeTiles rmm = loop (placeFirstTile rmm)
  where
    loop ptm = case placeTile rmm ptm of
      Nothing -> ptm
      Just ptm' -> loop ptm'
-- ^ if no placed tiles put the first one unflipped, unrotated, at (0,0)
-- find unmatched edges in placed tile map
-- if none then done
-- else use first unmatched edge to place a tile

-- edges that have been placed, have a match, but the match hasn't been placed.
placedEdges :: RMM -> PTM -> RMM
placedEdges rmm ptm = Map.filterWithKey lonely rmm
  where
    lonely (tid,_) (_, (tid',_)) = Map.member tid ptm && Map.notMember tid' ptm

-- transformEdgeId isFlipped rot eid gives the transformed edge ID, which
-- essentially tells you which direction the edge is pointing. Edge 0 is the top
-- edge, 1 is right, 2 is down, 3 is left. The transformed edge is after the
-- tile is flipped horizontally and then rotated rot quarter turns left
-- (counter-clockwise). An untransformed tile's edge IDs are like [0,1,2,3].
-- >>> transformEdgeId False 0 <$> [0,1,2,3]
-- [0,1,2,3]
-- >>> transformEdgeId False 1 <$> [0,1,2,3]
-- [3,0,1,2]
-- >>> transformEdgeId True 0 <$> [0,1,2,3]
-- [0,3,2,1]
-- >>> transformEdgeId True 1 <$> [0,1,2,3]
-- [3,2,1,0]
transformEdgeId :: Bool -> Int -> Int -> Int
transformEdgeId False rot eid = ( eid - rot) `mod` 4
transformEdgeId True  rot eid = (-eid - rot) `mod` 4

oppositeEdgeId :: Int -> Int
oppositeEdgeId eid = (eid + 2) `mod` 4

-- Give the sourceEid, whether it is flipped (in absolute terms), and what you
-- want its targetEid to be, calculate the required rotation as the number of
-- quarter turns left, such that transformEdgeId isFlipped (getRotTo isFlipped
-- targetEid sourceEid) sourceEid == targetEid.
-- >>> getRotTo False 2 <$> [0,1,2,3]
-- [2,3,0,1]
-- >>> getRotTo False 1 <$> [0,1,2,3]
-- [3,0,1,2]
-- >>> getRotTo True 2 <$> [0,1,2,3]
-- [2,1,0,3]
-- >>> getRotTo True 1 <$> [0,1,2,3]
-- [3,2,1,0]
getRotTo :: Bool -> Int -> Int -> Int
getRotTo False targetEid sourceEid = ( sourceEid - targetEid) `mod` 4
getRotTo True  targetEid sourceEid = (-sourceEid - targetEid) `mod` 4

dirFromEid :: Int -> (Int, Int)
dirFromEid eid = case eid of
  0 -> (0, -1) -- top points negative y
  1 -> (1, 0)  -- right points positive x
  2 -> (0, 1)
  3 -> (-1, 0)

addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos (a, b) (c, d) = (a+c, b+d)

getCorners :: MatchMap -> [Int]
getCorners mm = Map.keys $ Map.filter (== 2) $ matchCounts mm

matchCounts :: MatchMap -> Map.Map Int Int
matchCounts =
  Map.mapKeysWith (+) fst . Map.map (\(fms, rms) -> (length fms + length rms))

findMatches :: EMM -> Edge -> [(Int, Int)]
findMatches emm e =
  fromMaybe [] $ Map.lookup e emm

-- refined match map. if TEID has a match, it will be mapped to (isFlipped,
-- teid), where isFlipped True means the other tile has to flipped with respect
-- to this one.
type RMM = Map.Map TEID (Bool, TEID)

refineMatchMap :: MatchMap -> RMM
refineMatchMap = Map.mapMaybe refine
  where
    refine ([], []) = Nothing
    refine ([flippedTeid], []) = Just (True, flippedTeid)
    refine ([], [unflippedTeid]) = Just (False, unflippedTeid)
    refine _ = error "refineMatchMap: more than one match"

type TEID = (Int, Int)
type MatchMap = Map.Map TEID ([TEID], [TEID])

findAllMatches :: TM -> MatchMap
findAllMatches tm = Map.mapWithKey f em
  where
    em = edgeMap tm
    f teid e = (ff teid e, fr teid e)
    ff teid e = delete teid $ Map.keys $ Map.filter (== e) em
    fr teid e = delete teid $ Map.keys $ Map.filter (== reverse e) em

mkEMM :: TM -> EMM
mkEMM tm =
  let allEdges = [(e, (tid, eid)) | (tid, t) <- Map.toList tm
                                  , (eid, e) <- zip [0..] (tileEdges t) ]
      matched = foldl' f Map.empty allEdges
      f m (e, ids) = Map.insertWith (++) e [ids] m
  in  matched

type EMM = Map.Map Edge [(Int,Int)]

type Edge = [Bool]

showEdge :: Edge -> String
showEdge = map showSpot

edgeMap :: TM -> EM
edgeMap tm = Map.fromList $ concatMap f $ Map.toList tm
  where
    f (tid, t) = zipWith (\eid e -> ((tid, eid), e)) [0..] (tileEdges t)

tileEdges :: Tile -> [Edge]
tileEdges t = [top, right, bottom, left]
  where
    top = map (t CV.!) topCs
    right = map (t CV.!) rightCs
    bottom = map (t CV.!) bottomCs
    left = map (t CV.!) leftCs

topCs :: [(Int, Int)]
topCs = [(x,0) | x <- [0..9]]

rightCs :: [(Int, Int)]
rightCs = [(9,y) | y <- [0..9]]

bottomCs :: [(Int, Int)]
bottomCs = reverse [(x,9) | x <- [0..9]]

leftCs :: [(Int, Int)]
leftCs = reverse [(0,y) | y <- [0..9]]

type EM = Map.Map (Int, Int) Edge

type TM = Map.Map Int Tile
type Tile = CoordVec Bool

readTM :: [String] -> TM
readTM = Map.fromList . map readTile . splitOn [""]

readTile :: [String] -> (Int, Tile)
readTile (idStr:tileStr) =
  let id = read $ takeWhile isDigit $ drop 5 idStr
      tbs = (map.map) readSpot $ tileStr
  in  (id, CV.fromLists tbs)

readSpot :: Char -> Bool
readSpot '#' = True
readSpot '.' = False

showTM :: TM -> [String]
showTM tm =
  concatMap f $ Map.toList tm
  where
    f (id, t) = ("Tile " <> show id <> ":")
      : showTile t
      ++ [""]

showTile :: Tile -> [String]
showTile t =
  (map.map) showSpot $ CV.toLists t

showSpot :: Bool -> Char
showSpot True = '#'
showSpot False = '.'

{-
findMatches :: EMM -> Map.Map (Int, Int) [(Int, Int)]
findMatches = loop Map.empty
  where
    loop r emm =
      case Map.minViewWithKey emm of
        Nothing -> r
        Just ((edge, ids), emm') ->
          case Map.lookup (reverse edge) emm of
            Nothing ->
              let r' = foldl' (\m i -> Map.insert i [] m) r ids
                  emm'' = Map.delete (reverse edge) emm'
              in  loop r' emm''
            Just rids ->
              let r' = foldl' (\m i -> Map.insertWith (++) i rids m) r ids
                  r'' = foldl' (\m ri -> Map.insertWith (++) ri ids m) r' rids
                  emm'' = Map.delete (reverse edge) emm'
              in  loop r'' emm''
-}
