import Data.List
import Data.List.Split

parse :: String -> [Int]
parse = reverse . sort . map (sum . map read) . splitWhen null . lines

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ head input
    print $ sum $ take 3 input
