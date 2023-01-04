import Control.Monad.Extra
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Char
import Text.ParserCombinators.ReadP
import qualified Data.Set as S

type Coord = (Int, Int)
type Field = State (S.Set Coord)

int :: ReadP Int
int = read <$> (many1 $ satisfy isDigit)

coord :: ReadP Coord
coord = (,) <$> int <*> (char ',' *> int)

path, paths :: ReadP (S.Set Coord)
path = go <$> (sepBy coord $ string " -> ")
    where
        go [] = S.empty
        go [r] = S.singleton r
        go (r1 : rs@(r2 : _)) | r1 == r2 = go rs
        go (r@(x, y) : rs@((x', y') : _)) =
            S.insert r $ go $ (x + signum (x' - x), y + signum (y' - y)) : rs
paths = S.unions <$> (sepBy path $ char '\n')

step :: Coord -> Field (Maybe Coord)
step (x, y) =
    ifM (gets ((x    , y + 1) `S.notMember`)) (return $ Just (x    , y + 1)) (
    ifM (gets ((x - 1, y + 1) `S.notMember`)) (return $ Just (x - 1, y + 1)) (
    ifM (gets ((x + 1, y + 1) `S.notMember`)) (return $ Just (x + 1, y + 1)) (
        return Nothing
    )))

fallUntil :: Int -> Field Coord
fallUntil yMax = go (500, 0)
    where
        go r@(_, y)
            | y == yMax = return r
            | otherwise = do
                m <- step r
                case m of
                    Nothing -> r <$ modify (S.insert r)
                    Just r' -> go r'

heap :: Field [Coord]
heap = do
    yMax <- gets $ succ . maximum . S.map snd
    go yMax
    where
        go yMax = do
            r <- fallUntil yMax
            modify $ S.insert r
            fmap (r :) $ case r of
                (500, 0) -> return []
                _        -> go yMax

showTiles :: S.Set Coord -> S.Set Coord -> [[Char]]
showTiles walls sand =
    let xs = S.map fst $ walls `S.union` sand
        ys = S.map snd $ walls `S.union` sand
        xMin = S.findMin xs
        xMax = S.findMax xs
        yMin = S.findMin ys
        yMax = S.findMax ys
    in  [   [ if (x, y) `S.member` walls then '#' else
                  if (x, y) `S.member` sand  then 'o' else '.'
            | x <- [xMin .. xMax]
            ]
        | y <- [yMin .. yMax]
        ]

main :: IO ()
main = do
    walls <- fst . last . readP_to_S paths <$> getContents
    let rs = evalState heap walls
        yMax = maximum $ map snd rs
    print $ length $ takeWhile ((< yMax) . snd) rs
    print $ length rs
