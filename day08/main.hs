import Data.List

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f = go
    where
        go (x : xs)
            | f x       = [x]
            | otherwise = x : go xs
        go [] = []

grid :: String -> [[Int]]
grid = (map . map) (subtract (fromEnum '0') . fromEnum) . lines

left, right, above, below, visible :: [[Int]] -> [[Bool]]
left  = (zipWith . zipWith) (>) <$> id <*> map (       scanl max minBound)
right = (zipWith . zipWith) (>) <$> id <*> map (tail . scanr max minBound)
above = transpose . left  . transpose
below = transpose . right . transpose
visible g = foldl1 ((zipWith . zipWith) (||)) [left g, right g, above g, below g]

solve1 :: [[Int]] -> Int
solve1 = length . filter id . concat . visible

lefts, rights, aboves, belows :: [[Int]] -> [[[Int]]]
lefts  = map $ map reverse . inits
rights = map $ tail        . tails
aboves = transpose . lefts  . transpose
belows = transpose . rights . transpose

treesInDirection :: [[Int]] -> ([[Int]] -> [[[Int]]]) -> [[Int]]
treesInDirection g trees = (zipWith . zipWith) f g $ trees g
    where
        f n ns = length $ takeUntil (>= n) ns

solve2 :: [[Int]] -> Int
solve2 g = maximum $ concat $ foldl1 ((zipWith . zipWith) (*)) $ map (treesInDirection g) [lefts, rights, aboves, belows]

main :: IO ()
main = do
    input <- grid <$> getContents
    print $ solve1 input
    print $ solve2 input
