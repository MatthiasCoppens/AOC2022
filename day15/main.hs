{-# LANGUAGE TupleSections #-}

import Data.Char
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

type Coord = (Int, Int)

int :: ReadP Int
int = option id (negate <$ char '-') <*> (read <$> many1 (satisfy isDigit))

coord :: ReadP Coord
coord = (,) <$> (string "x=" *> int) <*> (string ", y=" *> int)

pair :: ReadP (Coord, Coord)
pair = (,)
    <$> (string "Sensor at " *> coord)
    <*> (string ": closest beacon is at " *> coord)

pairs :: ReadP [(Coord, Coord)]
pairs = sepBy pair (char '\n')

manhattan :: Coord -> Coord -> Int
manhattan (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)

rangesAtY :: Int -> Coord -> Coord -> [(Int, Int)]
rangesAtY y r0@(x0, y0) r1@(x1, y1) = case (manhattan r0 r1) - abs (y - y0) of
    dx  | dx < 0    -> []
        | y == y1   -> [(x0 - dx, x1 - 1), (x1 + 1, x0 + dx)]
        | otherwise -> [(x0 - dx, x0 + dx)]

rangeAtYFillBeacons :: Int -> Coord -> Coord -> Maybe (Int, Int)
rangeAtYFillBeacons y r0 r1 = case rangesAtY y r0 r1 of
    [] -> Nothing
    ps -> Just (fst (head ps), snd (last ps))

merge :: [(Int, Int)] -> [(Int, Int)]
merge = go . sort
    where
        go ((x0, x1) : (x2, x3) : rest)
            | x2 - x1 > 1 = (x0, x1) : go ((x2, x3) : rest)
            | otherwise   = go $ (x0, max x1 x3) : rest
        go ps = ps

solve1 :: Int -> [(Coord, Coord)] -> Int
solve1 y ps = sum $ map (\(x0, x1) -> x1 - x0 + 1) $ merge
    [range | (sensor, beacon) <- ps, range <- rangesAtY y sensor beacon]

solve2 :: [(Coord, Coord)] -> Int
solve2 ps = head
    [ 4000000 * x + y
    | y <- [0 ..]
    , let ps' = merge $ catMaybes
            [ rangeAtYFillBeacons y sensor beacon
            | (sensor, beacon) <- ps
            ]
    , ((_, x0), (x1, _)) <- zip ps' (tail ps')
    , x <- [x0 + 1 .. x1 - 1]
    ]

main :: IO ()
main = do
    input <- fst . last . readP_to_S pairs <$> getContents
    putStr "Part 1 at y = 10:      "
    print $ solve1 10 input
    putStr "Part 1 at y = 2000000: "
    print $ solve1 2000000 input
    putStr "Part 2:                "
    print $ solve2 input
