module Parse (Valve, valves) where

import Data.Char
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as M

type Valve = String

int :: ReadP Int
int = read <$> (many1 $ satisfy isDigit)

name :: ReadP Valve
name = many1 (satisfy (`notElem` "\n "))

valve :: ReadP (Valve, Int, [Valve])
valve = (,,)
    <$> (string "Valve " *> name)
    <*> (string " has flow rate=" *> int)
    <*> (
        string "; tunnel" *>
        optional (char 's') *>
        string " lead" *>
        optional (char 's') *>
        string " to valve" *>
        optional (char 's') *>
        string " " *>
        sepBy1 name (string ", "))

valves :: ReadP (M.Map Valve (Int, [Valve]))
valves = foldr (\(v, n, vs) -> M.insert v (n, vs)) M.empty <$>
    sepBy valve (char '\n')
