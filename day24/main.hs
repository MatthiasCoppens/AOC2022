{-# LANGUAGE TupleSections #-}

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Coord = (Int, Int)
type Blizzard = Coord
type Blizzards = M.Map Coord [Blizzard]
type Field = ReaderT Coord (State Blizzards)

parse :: String -> (Coord, Blizzards)
parse = addMoveBlizzard . M.fromList . concat . zipWith zip grid .
    map (map f . init . tail) . init . tail . takeWhile (not . null) . lines
    where
        f '^' = [(-1, 0)]
        f '>' = [(0, 1)]
        f 'v' = [(1, 0)]
        f '<' = [(0, -1)]
        f '.' = []
        grid = zipWith zip (map repeat [0 ..]) (repeat [0 ..])
        addMoveBlizzard = (,) <$> (fst . M.findMax) <*> id

moveBlizzards :: Field ()
moveBlizzards = do
    (maxRow, maxCol) <- ask
    lift $ modify $
        foldr (\(x, dx) -> M.insertWith (++) x [dx]) M.empty .
        map (\((r, c), (dr, dc)) ->
            (((r + dr) `mod` (maxRow + 1),
            (c + dc) `mod` (maxCol + 1)), (dr, dc))) .
        M.foldrWithKey (\x dxs -> (++) $ map (x,) dxs) []

empty :: Field (S.Set Coord)
empty = do
    (maxRow, maxCol) <- ask
    nonEmpty <- lift $ gets $ M.keysSet . M.filter (not . null)
    return $ S.fromList $ filter (`S.notMember` nonEmpty) $
        (-1, 0) :
        [ (r, c) | r <- [0 .. maxRow], c <- [0 .. maxCol]] ++
        [(maxRow + 1, maxCol)]

grow :: S.Set Coord -> S.Set Coord
grow xs = S.unions
    [ xs
    , S.mapMonotonic (\(r, c) -> (r + 1, c)) xs
    , S.mapMonotonic (\(r, c) -> (r - 1, c)) xs
    , S.mapMonotonic (\(r, c) -> (r, c + 1)) xs
    , S.mapMonotonic (\(r, c) -> (r, c - 1)) xs
    ]

step :: S.Set Coord -> Field (S.Set Coord)
step xs = do
    moveBlizzards
    (S.intersection $ grow xs) <$> empty

fromTo :: Coord -> Coord -> Field Int
fromTo start end = go $ S.singleton start
    where
        go xs
            | end `S.member` xs = return 0
            | otherwise         = succ <$> (step xs >>= go)

main :: IO ()
main = do
    ((maxRow, maxCol), blizzards) <- parse <$> getContents
    mapM_ print $ evalState (runReaderT (do
            n1 <- fromTo (-1, 0) (maxRow + 1, maxCol)
            n2 <- fromTo (maxRow + 1, maxCol) (-1, 0)
            n3 <- fromTo (-1, 0) (maxRow + 1, maxCol)
            return [n1, n1 + n2 + n3]
        ) (maxRow, maxCol)) blizzards
