import Control.Monad.State hiding (fix)
import qualified Data.Set as S

type Snake = [(Int, Int)]

parse :: String -> [(Char, Int)]
parse = map ((,) <$> head <*> read . drop 2) . lines

snake :: Int -> Snake
snake n = replicate n (0, 0)

fix :: State Snake (Int, Int)
fix = do
    s <- get
    case s of
        ((xH, yH) : (xT, yT) : rest)
            | (abs (xH - xT) <= 1) && (abs (yH - yT) <= 1) -> do
                modify tail
                ans <- fix
                modify ((xH, yH) :)
                return ans
            | xH == xT -> do
                let dy = (yH - yT) `div` abs (yH - yT)
                put $ (xH, yH) : (xT, yT + dy) : rest
                fix
            | yH == yT -> do
                let dx = (xH - xT) `div` abs (xH - xT)
                put $ (xH, yH) : (xT + dx, yT) : rest
                fix
            | otherwise -> do
                let dx = (xH - xT) `div` abs (xH - xT)
                    dy = (yH - yT) `div` abs (yH - yT)
                put $ (xH, yH) : (xT + dx, yT + dy) : rest
                fix
        [(x, y)] -> return (x, y)

move1 :: Char -> State Snake (Int, Int)
move1 c = do
    (x, y) <- state (\(x : xs) -> (x, xs))
    case c of
        'R' -> modify ((x + 1, y) :)
        'L' -> modify ((x - 1, y) :)
        'U' -> modify ((x, y + 1) :)
        'D' -> modify ((x, y - 1) :)
    fix

move :: (Char, Int) -> State Snake (S.Set (Int, Int))
move (c, n) = fmap S.fromList $ replicateM n $ move1 c

run :: [(Char, Int)] -> State Snake (S.Set (Int, Int))
run = fmap S.unions . mapM move

main :: IO ()
main = do
    program <- parse <$> getContents
    print $ S.size $ evalState (run program) $ snake 2
    print $ S.size $ evalState (run program) $ snake 10
