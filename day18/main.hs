import qualified Data.Set as S

type Coord = (Int, Int, Int)

parse :: String -> [Coord]
parse = map (read . ('(' :) . (++ ")")) . takeWhile (not . null) . lines

neighbours :: Coord -> S.Set Coord
neighbours (x, y, z) = S.fromAscList
    [ (x - 1, y, z), (x, y - 1, z), (x, y, z - 1)
    , (x, y, z + 1), (x, y + 1, z), (x + 1, y, z)
    ]

surface :: S.Set Coord -> Int
surface rs = S.foldr (\r -> (+ (S.size $ neighbours r S.\\ rs))) 0 rs

enclose :: Int -> Int -> S.Set Coord -> S.Set Coord
enclose minX maxX droplet = go S.empty [(minX, minX, minX)]
    where
        go negative ((x, y, z) : rest)
            | (x < minX) || (y < minX) || (z < minX) || (x > maxX) ||
                (y > maxX) || (z > maxX) || (x, y, z) `S.member` droplet ||
                (x, y, z) `S.member` negative = go negative rest
            | otherwise = go (S.insert (x, y, z) negative) (rest ++
                [ (x - 1, y, z), (x, y - 1, z), (x, y, z - 1)
                , (x, y, z + 1), (x, y + 1, z), (x + 1, y, z)
                ])
        go negative [] = negative

-- For part two, this assumes the droplet is contained in
--  -1 < x,y,z < 22
main :: IO ()
main = do
    rs <- S.fromList . parse <$> getContents
    print $ surface rs
    print $ subtract (6 * 24^2) $ surface $ enclose (-1) 22 rs
