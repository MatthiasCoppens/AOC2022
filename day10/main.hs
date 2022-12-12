{-# LANGUAGE LambdaCase #-}

import Data.Bool
import Data.Char
import Text.ParserCombinators.ReadP
import qualified Data.Set as S

data CMD = AddX Int | Noop

int :: ReadP Int
int = option id (negate <$ char '-') <*> (read <$> many1 (satisfy isDigit))

cmd :: ReadP CMD
cmd = (AddX <$> (string "addx " *> int)) +++ (Noop <$ string "noop")

cmds :: ReadP [CMD]
cmds = sepBy cmd (char '\n')

run :: [CMD] -> [Int]
run = go 1
    where
        go x = \case
            ((AddX v) : rest) -> x : x : go (x + v) rest
            (Noop : rest)     -> x : go x rest
            []                -> [x]

crt :: [Int] -> String
crt = map (bool ' ' '#') .
    zipWith (\px sprite -> (sprite - 1 <= px `mod` 40) && (px `mod` 40 <= sprite + 1)) [0 ..]

main :: IO ()
main = do
    program <- fst . last . readP_to_S cmds <$> getContents
    let xs = run program
    print $ sum [i * xs !! (i - 1) | i <- [20, 60, 100, 140, 180, 220]]
    putStrLn $ crt xs
