import Data.Char
import qualified Data.Map.Strict as M

type Coord = (Int, Int)

grid :: [[Char]] -> M.Map Coord Char
grid =
    M.filter (`elem` "#.") .
    M.fromList .
    concat .
    (zipWith . zipWith) (,) (
        (zipWith . zipWith) (,) (repeat [1 ..]) (map repeat [1 ..]))

directions :: String -> [Either Char Int]
directions "" = []
directions ('R' : rest) = Left 'R' : directions rest
directions ('L' : rest) = Left 'L' : directions rest
directions s =
    (Right $ read $ takeWhile isDigit s) : (directions $ dropWhile isDigit s)

parse :: String -> (M.Map Coord Char, [Either Char Int])
parse = ((,) <$> (grid . init) <*> (directions . last)) . init . lines

step :: M.Map Coord Char -> Coord -> Coord -> Maybe Coord
step tiles (dx, dy) (x, y) =
    let x' = x + dx
        y' = y + dy
    in  case tiles M.!? (x', y') of
            Just '#' -> Nothing
            Just '.' -> Just (x', y')
            Nothing  -> step tiles (dx, dy) $ head $
                dropWhile (`M.member` tiles) $
                iterate (\(x, y) -> (x - dx, y - dy)) (x, y)

turn :: Char -> Coord -> Coord
turn 'R' (dx, dy) = (-dy, dx)
turn 'L' (dx, dy) = (dy, -dx)

run :: M.Map Coord Char -> [Either Char Int] -> Coord -> Coord -> Int
run tiles = go
    where
        go (Left c : dirs) d r = go dirs (turn c d) r
        go (Right 0 : dirs) d r = go dirs d r
        go (Right n : dirs) d r = case step tiles d r of
            Nothing -> go dirs d r
            Just r' -> go (Right (n - 1) : dirs) d r'
        go [] d (x, y) = case d of
            (1, 0)  -> 1000 * y + 4 * x + 0
            (0, 1)  -> 1000 * y + 4 * x + 1
            (-1, 0) -> 1000 * y + 4 * x + 2
            (0, -1) -> 1000 * y + 4 * x + 3

main :: IO ()
main = do
    (g, ds) <- parse <$> getContents
    print $ run g ds (1, 0) $ head $ dropWhile (`M.notMember` g)
        [(x, 1) | x <- [1 ..]]
