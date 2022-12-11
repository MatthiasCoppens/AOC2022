{-# LANGUAGE TupleSections #-}

import Control.Monad.State as S
import Data.Char
import Data.Maybe
import Data.List
import Text.ParserCombinators.ReadP as R
import qualified Data.Map.Strict as M

type Worry = Int
type Index = Word
type Monkey = ([Worry], Worry -> Worry, Worry -> Index)
type Stack a = State [a]

parse :: ReadP a -> String -> a
parse r = fst . last . readP_to_S r

num :: (Num a, Read a) => ReadP a
num = read <$> many1 (satisfy isDigit)

startingItems :: ReadP [Worry]
startingItems = string "  Starting items: " *> sepBy num (string ", ")

op :: ReadP (Worry -> Worry -> Worry)
op = ((+) <$ string " + ") +++ ((*) <$ string " * ")

var :: ReadP (Maybe Worry)
var = (Nothing <$ string "old") +++ (Just <$> num)

-- Parse function: old -> new
operation :: ReadP (Worry -> Worry)
operation = do
    string "  Operation: new = "
    l <- var
    o <- op
    r <- var
    return $ \old -> fromMaybe old l `o` fromMaybe old r

test :: ReadP (Worry, (Worry -> Bool))
test = do
    string "  Test: divisible by "
    modulus <- num
    return (modulus, (== 0) . (`mod` modulus))

throwStatement :: ReadP (Bool, Index)
throwStatement = do
    string "    If "
    b <- (True <$ string "true") +++ (False <$ string "false")
    string ": throw to monkey "
    (b, ) <$> num

-- Parse function: new -> monkey to throw to
throw :: ReadP (Worry, Worry -> Index)
throw = do
    (modulus, t) <- test <* char '\n'
    (b1, n1) <- throwStatement <* char '\n'
    (_ , n2) <- throwStatement <* char '\n'
    return (modulus, \n -> if t n == b1 then n1 else n2)

monkey :: ReadP (Worry, Monkey)
monkey = do
    string "Monkey " *> R.get *> string ":\n"
    items <- (startingItems <* char '\n')
    f1 <- (operation <* char '\n')
    (modulus, f2) <- throw
    return (modulus, (items, f1, f2))

-- Parse complete input
monkeys :: ReadP (Worry, [Monkey])
monkeys = do
    (moduli, ms) <- unzip <$> sepBy monkey (char '\n')
    return (foldl1 lcm moduli, ms)

push :: a -> Stack a ()
push = modify . (:)

pop :: Stack a a
pop = state $ \(x : xs) -> (x, xs)

-- Monkey indices for n rounds
rounds :: Int -> Stack Monkey [Index]
rounds 1 = zipWith const [0 ..] <$> S.get
rounds n = concat . replicate n <$> rounds 1

-- Get objects and functions from monkey i, removing the objects from the monkey
getMonkey :: Index -> Stack Monkey Monkey
getMonkey i = do
    m@(_, f1, f2) <- pop
    case i of
        0 -> push ([], f1, f2) *> return m
        _ -> getMonkey (i - 1) <* push m

-- Throw object with worry level n to monkey i
throwTo :: Index -> Worry -> Stack Monkey ()
throwTo i n = go i
    where
        go i = do
            m@(ns, f1, f2) <- pop
            case i of
                0 -> push (ns ++ [n], f1, f2)
                _ -> go (i - 1) <* push m

run :: Worry -> Worry -> [Index] -> Stack Monkey [Index]
run modulus divWorry (i : rest) = do
    (ns, f1, f2) <- getMonkey i
    is <- mapM (\n ->
        let new = (f1 n `div` divWorry) `mod` modulus
            toMonkey = f2 new
        in  i <$ throwTo toMonkey new
        ) ns
    (is ++) <$> run modulus divWorry rest
run _ _ [] = return []

-- Run n rounds
runRounds :: Int -> Worry -> Worry -> Stack Monkey [Index]
runRounds n modulus divWorry = rounds n >>= run modulus divWorry

freqs :: (Foldable t, Ord a) => t a -> M.Map a Int
freqs = foldr (\x -> M.insertWith (+) x 1) M.empty

solve1, solve2 :: (Worry, [Monkey]) -> Int
solve1 (modulus, monkeys) =
    product $ take 2 $ reverse $ sort $ M.elems $ freqs $
    evalState (runRounds 20 modulus 3) monkeys
solve2 (modulus, monkeys) =
    product $ take 2 $ reverse $ sort $ M.elems $ freqs $
    evalState (runRounds 10000 modulus 1) monkeys

main :: IO ()
main = do
    input <- parse monkeys <$> getContents
    print $ solve1 input
    print $ solve2 input
