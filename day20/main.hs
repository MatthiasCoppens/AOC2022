import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.List
import Data.Tuple
import qualified Data.Map.Strict as M

type Ring = StateT (M.Map Int Int) (ReaderT (M.Map Int Int) (Reader Int))

ints :: String -> [Int]
ints = map read . takeWhile (not . null) . lines

evalRing :: [Int] -> Ring a -> a
evalRing ns x =
    runReader (
        runReaderT (
            evalStateT x (M.fromList $ zipWith (\i _ -> (i, i)) [0 ..] ns)
            ) (M.fromList $ zip [0 ..] ns)
        ) (length ns)

nums :: Ring Int
nums = lift $ lift $ ask

num :: Int -> Ring Int
num i = lift $ asks (M.! i)

ring :: Ring [Int]
ring = do
    is <- gets $ map snd . sort . map swap . M.toList
    ns <- mapM num is
    return $ dropWhile (/= 0) $ cycle ns

remove :: Int -> Ring Int
remove i = do
    p <- gets (M.! i)
    modify $ M.delete i
    modify $ M.map $ \p' -> if p' < p then p' else p' - 1
    return p

add :: Int -> Int -> Ring ()
add i p = do
    modify $ M.map $ \p' -> if p' < p then p' else p' + 1
    modify $ M.insert i p

newPlace :: Int -> Int -> Ring Int
newPlace p n = (((p + n) `mod`) . pred) <$> nums

move :: Int -> Ring ()
move i = do
    p <- remove i
    num i >>= newPlace p >>= add i

mix :: Ring ()
mix = do
    l <- nums
    mapM_ move [0 .. l - 1]

main :: IO ()
main = do
    ns <- ints <$> getContents
    let ns1 = evalRing                    ns  (              mix *> ring)
        ns2 = evalRing (map (* 811589153) ns) (replicateM 10 mix *> ring)
    mapM_ (print . sum . map head . take 3 . tail . iterate (drop 1000))
        [ns1, ns2]
