add :: Int -> Int -> Int
add x y = (x + y) `mod` 100

scanList :: [Int] -> [Int]
scanList = scanl add 50

parse :: String -> Int
parse s = parseDirection (head s) * read (tail s)

parseDirection :: Char -> Int
parseDirection a = case a of
    'L' -> -1
    'R' -> 1
    _   -> error "Invalid direction"

translate :: [String] -> [Int]
translate = map parse

countZeros :: [Int] -> Int
countZeros = length . filter (== 0)

solve :: [String] -> Int
solve = countZeros . scanList . translate

main :: IO ()
main = do
    contents <- readFile "q1.txt"
    print (solve (lines contents))