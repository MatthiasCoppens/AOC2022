import Data.Char
import Text.ParserCombinators.ReadP

int :: ReadP Int
int = fmap read $ many1 $ satisfy isDigit

range :: ReadP (Int, Int)
range = (,) <$> int <*> (char '-' *> int)

pair :: ReadP ((Int, Int), (Int, Int))
pair = (,) <$> range <*> (char ',' *> range)

pairs :: ReadP [((Int, Int), (Int, Int))]
pairs = sepBy pair (char '\n')

solve1, solve2 :: [((Int, Int), (Int, Int))] -> Int

solve1 = length . filter f
    where
        f ((l1, r1), (l2, r2)) = case compare l1 l2 of
            EQ  -> True
            cmp -> cmp /= compare r1 r2

solve2 = length . filter f
    where
        f ((l1, r1), (l2, r2)) = min r1 r2 >= max l1 l2

main :: IO ()
main = do
    input <- fst . last . readP_to_S pairs <$> getContents
    print $ solve1 input
    print $ solve2 input
