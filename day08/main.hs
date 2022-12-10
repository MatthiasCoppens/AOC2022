import Data.List

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

main :: IO ()
main = do
    input <- grid <$> getContents
    print $ solve1 input
