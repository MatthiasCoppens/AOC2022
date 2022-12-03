import qualified Data.Map.Strict as M
import qualified Data.Set        as S

scores :: M.Map Char Int
scores = M.fromList $ zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1 ..]

parse :: String -> [String]
parse = takeWhile (not . null) . lines

solve1 :: [String] -> Int
solve1 = sum . map (
        (scores M.!) .
        S.findMin .
        (\(x, y) -> S.fromList x `S.intersection` S.fromList y) .
        split2
    )
    where
        split2 s = splitAt (length s `div` 2) s

solve2 :: [String] -> Int
solve2 [] = 0
solve2 elves =
    let (group, rest) = splitAt 3 elves
        score = (scores M.!) $ S.findMin $ foldl1 S.intersection $
            map S.fromList group
    in  score + solve2 rest

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    print $ solve2 input
