{-# LANGUAGE TupleSections #-}

import Control.Monad.State
import Data.Char
import Data.Ratio
import Text.ParserCombinators.ReadP hiding (get)
import qualified Data.Map.Strict as M

type Name = String
data Op =
      Const Int
    | Var Name
    | (:+:) Name Name
    | (:-:) Name Name 
    | (:*:) Name Name 
    | (:/:) Name Name 
data Eval = Evaluated Solution | Unevaluated Op
type Vars = M.Map Name Eval
type Solution = Ratio Int

name :: ReadP Name
name = many1 $ satisfy (/= '\n')

int :: ReadP Int
int = read <$> (many1 $ satisfy isDigit)

op :: ReadP Op
op = (Const <$> int) +++ (
    do
        l <- name <* char ' '
        o <- choice $ zipWith (<$) [(:+:), (:-:), (:*:), (:/:)] $ map char "+-*/"
        r <- char ' ' *> name
        return $ l `o` r
    )

vars :: ReadP Vars
vars = M.fromList <$>
    sepBy ((,) <$> (name <* string ": ") <*> (Unevaluated <$> op)) (char '\n')

-- Code that's commented out does memoization.
-- Since the input is pretty small, memoization adds to much overhead,
-- making the code slower.
eval :: Op -> State Vars Solution
eval (Const n) = return $ n % 1
eval (Var x) = do
    ans <- gets (M.! x)
    case ans of
        Evaluated n   -> return n
        Unevaluated e -> do
            n <- eval e
            -- modify $ M.insert x $ Evaluated n
            return n
eval (l :+: r) = (+) <$> eval (Var l) <*> eval (Var r)
eval (l :-: r) = (-) <$> eval (Var l) <*> eval (Var r)
eval (l :*: r) = (*) <$> eval (Var l) <*> eval (Var r)
eval (l :/: r) = (/) <$> eval (Var l) <*> eval (Var r)

rootWithHumn :: Int -> State Vars Solution
rootWithHumn humn = do
    backup <- get
    modify $ M.insert "humn" $ Unevaluated $ Const humn
    (eval $ Var "root") <* put backup

solve1 :: State Vars Int
solve1 = round <$> eval (Var "root")
solve2 = do
    root <- gets (M.! "root")
    let root' = case root of
            Unevaluated (l :+: r) -> Unevaluated (l :-: r)
            Unevaluated (l :-: r) -> Unevaluated (l :-: r)
            Unevaluated (l :*: r) -> Unevaluated (l :-: r)
            Unevaluated (l :/: r) -> Unevaluated (l :-: r)
    modify $ M.insert "root" root'
    go (-1) 1
    where
        go humnA humnZ = do
            rootA <- rootWithHumn humnA
            rootZ <- rootWithHumn humnZ
            let humn = (humnA + humnZ) `div` 2
            root <- rootWithHumn humn
            case (compare rootA 0, compare rootZ 0) of
                (EQ, _) -> return humnA
                (_, EQ) -> return humnZ
                (a, b)
                    | a > b  -> go humnZ humnA
                    | a == b -> go (2 * humnA - humnZ) (2 * humnZ - humnA)
                    | otherwise -> case compare root 0 of
                        EQ -> return humn
                        LT -> go humn humnZ
                        GT -> go humnA humn

main :: IO ()
main = do
    vs <- fst . last . readP_to_S vars <$> getContents
    print $ evalState solve1 vs
    print $ evalState solve2 vs
