import Data.Char
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as M

data Dir = Root | Parent | Child String deriving Show
data CMD = CD Dir | LS [(Maybe Int, String)] deriving Show
data FileSystem = Directory String [FileSystem] | File Int String

instance Show FileSystem where
    show = unlines . go
        where
            go :: FileSystem -> [String]
            go (Directory dirName children) = ("- " ++ dirName ++ " (dir)") : map ("  " ++) (concatMap go children)
            go (File size fileName) = ["- " ++ fileName ++ " (file, size=" ++ show size ++ ")"]

filename :: ReadP String
filename = many1 $ satisfy (`elem` (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['.']))

int :: ReadP Int
int = read <$> (many1 $ satisfy isDigit)

dir :: ReadP Dir
dir = (Root <$ char '/') +++ (Parent <$ string "..") <++ (Child <$> filename)

cd :: ReadP CMD
cd = CD <$> (string "$ cd " *> dir)

lsOutputLine :: ReadP (Maybe Int, String)
lsOutputLine = (,)
    <$> ((Nothing <$ string "dir") +++ (Just <$> int))
    <*> (char ' ' *> filename)

ls :: ReadP CMD
ls = LS <$> (string "$ ls\n" *> sepBy lsOutputLine (char '\n'))

cmds :: ReadP [CMD]
cmds = sepBy (cd +++ ls) (char '\n')

run :: FileSystem -> [CMD] -> (FileSystem, [CMD])
run fs [] = (fs, [])
run fs (cmds@(CD Root : _)) = (fs, cmds)
run fs (CD Parent : cmds) = (fs, cmds)
run (Directory dirName (fs@(Directory child _) : rest)) (CD (Child child') : cmds)
    | child == child' = 
        let (fs', cmds') = run fs cmds
        in  run (Directory dirName (fs' : rest)) cmds'
run (Directory dirName (x : xs)) cmds@(CD (Child _) : _) = run (Directory dirName (xs ++ [x])) cmds
run (Directory dirName _) (LS list : rest) = run (Directory dirName (map f list)) rest
    where
        f (Nothing, dirName) = Directory dirName []
        f (Just size, fileName) = File size fileName
run fs (cmd : _) = error $ '\n' : show fs ++ '\n' : show cmd

size :: FileSystem -> Int
size (Directory _ children) = sum $ size <$> children
size (File s _) = s

dirs :: FileSystem -> [(Int, String)]
dirs d@(Directory dirName children) = (size d, dirName) : concatMap dirs children
dirs _ = []

solve1, solve2 :: FileSystem -> Int
solve1 = sum . filter (<= 100000) . map fst . dirs
solve2 fs = minimum $ filter (>= (30000000 - 70000000 + size fs)) $ map fst $ dirs fs

main :: IO ()
main = do
    input <- tail . fst . last . readP_to_S cmds <$> getContents
    let fs = fst $ run (Directory "/" []) input
    print $ solve1 fs
    print $ solve2 fs
