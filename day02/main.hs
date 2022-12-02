data Shape = Rock | Paper | Scissors deriving (Enum, Eq, Show)

instance Ord Shape where
    compare x        y        | x == y = EQ
    compare Rock     Paper             = LT
    compare Paper    Scissors          = LT
    compare Scissors Rock              = LT
    compare _        _                 = GT

succ', pred' :: Shape -> Shape
succ' = toEnum . (`mod` 3) . succ . fromEnum
pred' = toEnum . (`mod` 3) . pred . fromEnum

parse :: String -> [(Shape, Shape)]
parse = (zip <$> map (l . head) <*> map (r . last)) . init . lines
    where
        l 'A' = Rock
        l 'B' = Paper
        l 'C' = Scissors
        r 'X' = Rock
        r 'Y' = Paper
        r 'Z' = Scissors

score :: Shape -> Shape -> Int
score x y =
    let score1 = succ        $ fromEnum             y
        score2 = (*3) $ (2-) $ fromEnum $ compare x y
    in  score1 + score2

fix :: Shape -> Shape -> (Shape, Shape)
fix x = (,) x . f
    where
        f Rock     = pred' x
        f Paper    =       x
        f Scissors = succ' x

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ sum $ map (uncurry score              ) input
    print $ sum $ map (uncurry score . uncurry fix) input
