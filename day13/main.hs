import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

data Packet = Node [Packet] | Leaf Int

instance Show Packet where
    show (Node ps) = '[' : intercalate "," (map show ps) ++ "]"
    show (Leaf n)  = show n

instance Eq Packet where
    (==) l r = compare l r == EQ

instance Ord Packet where
    compare (Leaf l) (Leaf r) = compare l r
    compare (Node (l : ls)) (Node (r : rs)) = case compare l r of
        EQ  -> compare (Node ls) (Node rs)
        cmp -> cmp
    compare (Node []) (Node []) = EQ
    compare (Node []) (Node _) = LT
    compare (Node _) (Node []) = GT
    compare l@(Leaf _) r = compare (Node [l]) r
    compare l r@(Leaf _) = compare l (Node [r])

int :: ReadP Int
int = read <$> many1 (satisfy isDigit)

packet :: ReadP Packet
packet = (Node <$> (char '[' *> sepBy packet (char ',') <* char ']')) +++
    (Leaf <$> int)

pair :: ReadP (Packet, Packet)
pair = (,) <$> (packet <* char '\n') <*> packet

signal :: ReadP [(Packet, Packet)]
signal = sepBy pair (string "\n\n")

solve1 :: [(Packet, Packet)] -> Int
solve1 = sum . map fst . filter (\(n, (l, r)) -> l < r) . zip [1 ..]
solve2 pps =
    let ps = sort $ foldr (\(l, r) -> ([l, r] ++)) [] pps
    in  (length (takeWhile (< Leaf 2) ps) + 1) *
            (length (takeWhile (< Leaf 6) ps) + 2)

main :: IO ()
main = do
    input <- fst . last . readP_to_S signal <$> getContents
    print $ solve1 input
    print $ solve2 input
