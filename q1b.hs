-- given two numbers, I need the result of
    -- plus how many zeros are in the result


-- 0 - 99, 4 - 400 = 

-- point at 0 NOT go over the boundary thing

-- do we make it to 0/100 then add what's left / 100

add x y = (x + y) `mod` 100

add2 (value, runningCount) y = (add value y, runningCount + count value y)

count x y = if y < 0 then count2 (abs y - (if x == 0 then 100 else x)) else count2 (y - (100 - x))
count2 x = if x < 0 then 0 else x `div` 100 + 1

foldList = foldl add2 (50, 0)

scanList = scanl add2 (50, 0)

parse s = parseDirection (head s) * read (tail s)
parseDirection a = case a of
    'L' -> -1
    'R' -> 1
    _   -> error "Invalid direction"

translate = map parse

solve = foldList . translate

main :: IO ()
main = do
    contents <- readFile "q1.txt"
    print (snd . solve . lines $ contents)
    print (solve ["L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82"])
    print (scanList . translate $ ["L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82"])