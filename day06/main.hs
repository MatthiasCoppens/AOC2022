import Data.List hiding (group)

group :: Int -> [a] -> [[a]]
group n = transpose . take n . tails

marker :: Eq a => Int -> [a] -> Int
marker n = (+ n) . length . takeWhile ((< n) . length) . map nub . group n

main :: IO ()
main = do
    input <- getContents
    print $ marker  4 input
    print $ marker 14 input
