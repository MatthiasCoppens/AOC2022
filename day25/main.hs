toDec :: String -> Int
toDec = foldl (\n c -> 5 * n + translate c) 0
    where
        translate '2' =  2
        translate '1' =  1
        translate '0' =  0
        translate '-' = -1
        translate '=' = -2

fromDec :: Int -> String
fromDec 0 = "0"
fromDec n = go n
    where
        go 0 = ""
        go n = case n `mod` 5 of
            2 -> go ((n - 2) `div` 5) ++ "2"
            1 -> go ((n - 1) `div` 5) ++ "1"
            0 -> go ( n      `div` 5) ++ "0"
            4 -> go ((n + 1) `div` 5) ++ "-"
            3 -> go ((n + 2) `div` 5) ++ "="

main :: IO ()
main = do
    ns <- map toDec . lines <$> getContents
    putStrLn $ fromDec $ sum ns
    putStrLn "Merry Christmas!"
