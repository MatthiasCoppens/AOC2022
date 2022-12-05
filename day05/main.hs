import Control.Monad.State.Lazy hiding (get)
import Data.Char
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

crate :: ReadP (Maybe Char)
crate = (Just <$> (char '[' *> get <* char ']')) +++ (Nothing <$ string "   ")

crates :: ReadP [[Char]]
crates = map catMaybes . transpose <$> sepBy (sepBy crate (char ' ')) (char '\n')

int :: ReadP Int
int = read <$> many1 (satisfy isDigit)

line :: ReadP String
line = many (satisfy (/= '\n')) <* char '\n'

movement :: ReadP (Int, Int, Int)
movement = (,,)
    <$> (string "move "  *> int)
    <*> (string " from " *> int)
    <*> (string " to "   *> int)

procedure :: ReadP ([[Char]], [(Int, Int, Int)])
procedure = (,) <$> (crates <* line <* line) <*> sepBy movement (char '\n')

getCrates :: Int -> Int -> [[a]] -> ([a], [[a]])
getCrates amount = go
    where
        go i (xs : xss) = case i of
            1 -> let (xs', xs'') = splitAt amount xs in (xs', xs'' : xss)
            _ -> let (xs', xss') = go (i - 1) xss in (xs', xs : xss')

putCrates :: Int -> [a] -> [[a]] -> [[a]]
putCrates i xs' = go i
    where
        go i (xs : xss) = case i of
            1 -> (xs' ++ xs) : xss
            _ -> xs : go (i - 1) xss

mover9000, mover9001 :: (Int, Int, Int) -> State [[a]] ()

mover9000 (amount, from, to) = replicateM_ amount $ do
    xs <- state $ getCrates 1 from
    modify $ putCrates to xs

mover9001 (amount, from, to) = do
    xs <- state $ getCrates amount from
    modify $ putCrates to xs

main :: IO ()
main = do
    (cratesStart, movements) <- fst . last . readP_to_S procedure <$> getContents
    putStrLn $ map head $ execState (mapM_ mover9000 movements) cratesStart
    putStrLn $ map head $ execState (mapM_ mover9001 movements) cratesStart
