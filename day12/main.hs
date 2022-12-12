import Control.Monad
import qualified Data.Map.Strict as M

type Coord = (Int, Int)
data Height = Start | Height Char | End deriving (Eq, Ord, Show)
type Grid = M.Map Coord Height

instance Enum Height where
    toEnum  0 = Start
    toEnum 27 = End
    toEnum  n = Height $ toEnum $ n + 96
    fromEnum  Start     = 0
    fromEnum  End       = 27
    fromEnum (Height c) = subtract 96 $ fromEnum c

coords :: [[Coord]]
coords = zipWith zip (map repeat [0 ..]) (repeat [0 ..])

height :: Char -> Height
height 'S' = Start
height 'E' = End
height  c  = Height c

grid :: String -> Grid
grid = M.fromList . concat . zipWith zip coords . (map . map) height . lines

neighbours :: Grid -> Grid
neighbours g = M.unionsWith max
    [ M.mapKeysMonotonic (\(x, y) -> (x `op` dx, y `op` dy)) g
    | (dx, dy) <- [(1, 0), (0, 1)], op <- [(-), (+)]
    ]

constrain :: Grid -> Grid -> (Grid, Grid)
constrain g0 g =
    let g' = M.mergeWithKey
            (\_ h0 h -> guard (succ h >= h0) *> Just h0)
            (const M.empty) (const M.empty) g0 g
        g0' = g0 M.\\ g'
    in  (g0', g')

shortestPath :: (Height -> Bool) -> Grid -> Int
shortestPath test g0 = go g0 $ M.filter test g0
    where
        go g0 g
            | End `elem` g = 0
            | otherwise    = succ $ uncurry go $ constrain g0 $ neighbours g

main :: IO ()
main = do
    input <- grid <$> getContents
    print $ shortestPath (== Start) input
    print $ shortestPath (== Height 'a') input
