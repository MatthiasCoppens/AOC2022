import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Coord = (Int, Int)
data Dir = North | South | West | East

grid :: String -> S.Set Coord
grid s = S.fromAscList
    [ (row, col)
    | (row, cs) <- zip [0 ..] (lines s)
    , (col, '#') <- zip [0 ..] cs
    ]

move1 :: [Dir] -> S.Set Coord -> Coord -> Coord
move1 dirs g (r, c)
    | g `S.disjoint` (S.fromAscList
        [(r + dr, c + dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], (dr, dc) /= (0, 0)]
        ) = (r, c)
    | otherwise = go dirs
    where
        go (North : dirs)
            | g `S.disjoint` (S.fromAscList [(r-1, c-1), (r-1, c), (r-1, c+1)])
                = (r-1, c)
            | otherwise = go dirs
        go (South : dirs)
            | g `S.disjoint` (S.fromAscList [(r+1, c-1), (r+1, c), (r+1, c+1)])
                = (r+1, c)
            | otherwise = go dirs
        go (West : dirs)
            | g `S.disjoint` (S.fromAscList [(r-1, c-1), (r, c-1), (r+1, c-1)])
                = (r, c-1)
            | otherwise = go dirs
        go (East : dirs)
            | g `S.disjoint` (S.fromAscList [(r-1, c+1), (r, c+1), (r+1, c+1)])
                = (r, c+1)
            | otherwise = go dirs
        go [] = (r, c)

moveNoCollisionCheck :: [Dir] -> S.Set Coord -> M.Map Coord Coord
moveNoCollisionCheck dirs g = M.fromSet (move1 dirs g) g

invert :: (Ord a, Ord b) => M.Map a b -> M.Map b (S.Set a)
invert = M.foldrWithKey (\k v -> M.insertWith S.union v (S.singleton k)) M.empty

-- First a create map (M.Map fromCoord toCoord),
-- then a map (M.Map toCoord fromCoordSet).
-- The next set is the union of the toCoords with (S.size fromCoordSet == 1)
-- with the fromCoordSets with size > 1.
move :: [Dir] -> S.Set Coord -> S.Set Coord
move dirs g =
    let m = invert $ moveNoCollisionCheck dirs g
        (m', m'') = M.partition ((== 1) . S.size) m
    in  S.unions $ M.keysSet m' : M.elems m''

moves :: S.Set Coord -> [S.Set Coord]
moves = go [North, South, West, East]
    where
        go (d : ds) g = g : go (ds ++ [d]) (move (d : ds) g)

border :: S.Set Coord -> (Coord, Coord)
border g =
    let rs = S.map fst g
        cs = S.map snd g
    in  ((S.findMin rs, S.findMin cs), (S.findMax rs, S.findMax cs))

draw :: S.Set Coord -> [String]
draw g =
    [   [ if (r, c) `S.member` g then '#' else '.'
        | c <- [cMin .. cMax]
        ]
    | let ((rMin, cMin), (rMax, cMax)) = border g
    , r <- [rMin .. rMax]
    ]

emptyTiles :: S.Set Coord -> Int
emptyTiles g =
    let ((rMin, cMin), (rMax, cMax)) = border g
        n = S.size g
    in  (rMax - rMin + 1) * (cMax - cMin + 1) - n

difs :: Eq a => [a] -> [a]
difs (x : x' : _) | x == x' = [x]
difs (x : xs)               = x : difs xs

main :: IO ()
main = do
    gs <- moves . grid <$> getContents
    print $ emptyTiles $ gs !! 10
    print $ length $ difs gs
