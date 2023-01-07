import Parse

import Data.Either
import Data.List
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as M

data Tree a = Tree a [Tree a] deriving Eq

instance Ord a => Ord (Tree a) where
    compare (Tree l _) (Tree r _) = compare l r

tree :: M.Map Valve (Int, [Valve]) -> Tree (Either Int Valve)
tree = goto "AA"
    where
        goto valve m = Tree (Right valve) $ next valve m
        open valve m =
            let (n, vs) = m M.! valve
            in  Tree (Left n) $ next valve $ M.insert valve (0, vs) m
        next _ m | all (== 0) (M.map fst m) = []
        next valve m = open valve m :
            [goto valve' m | let (_, valves) = m M.! valve, valve' <- valves]

prune :: Eq a => Tree (Either Int a) -> Tree (Either Int a)
prune = go []
    where
        go xs (Tree e children) =
            let xs' = case e of
                    Left _  ->     []
                    Right x -> x : xs
            in  Tree e
                    [ go xs' child
                    | child@(Tree e' _) <- children
                    , either (/= 0) (`notElem` xs') e'
                    ]

scores :: Int -> Tree (Either Int a) -> Tree Int
scores = go 0
    where
        go n t (Tree e children) =
            let n' = n + t * fromLeft 0 e
            in  Tree n' $ map (go n' (t - 1)) children

bruteForce :: Int -> Tree a -> Tree a
bruteForce 0 t = t
bruteForce n (Tree x children) = bruteForce (n - 1) $ Tree x
    [ grandchild
    | (Tree _ grandchildren) <- children
    , grandchild <- grandchildren
    ]

dfs :: Ord a => Tree a -> [a]
dfs (Tree x []) = [x]
dfs (Tree _ children) = concatMap dfs $ reverse $ sort children

scanMax :: Ord a => [a] -> [a]
scanMax (x : xs) = (x :) $ scanMax $ dropWhile (<= x) xs
scanMax [] = []

cutOff :: Int -> Tree a -> Tree a
cutOff 1 (Tree x _) = Tree x []
cutOff n (Tree x children) = Tree x $ map (cutOff (n - 1)) children

-- Presumably it'd take between 10 and 25 minutes to open all valves.
-- Only paths taking 25 minutes at most are calculated, and the first 10
-- minutes are bruteforced to kickstart the DFS.
main :: IO ()
main = do
    graph <- fst . last . readP_to_S valves <$> getContents
    mapM_ print $ scanMax $ dfs $ bruteForce 10 $ cutOff 25 $ scores 30 $ prune $ tree graph
