import Data.Char
import Data.List
import Data.Maybe

-- module Main (main) where

-- getInitials :: String -> String
-- getInitials xs = [(toUpper $ head $ head $ head words xs), '.', (toUpper $ head $ last $ words xs)]

list :: [Integer]
list = [1, 2, 2]


findSmallestInteger :: [Int] -> Int
findSmallestInteger = minimum
-- findSmallestInteger xs = minimum xs


squareSum :: [Integer] -> Integer
squareSum [] = 0
squareSum (x:xs) = (x * x) + squareSum xs
-- squareSum x = sum $ map (^2) x
-- squareSum x = sum [y^2 | y <- x ]

fakeBin :: String -> String
fakeBin [] = []
fakeBin (x:xs)| digitToInt x < 5 = '0' : fakeBin xs
            | otherwise = '1' : fakeBin xs



-- countSheep :: Int -> String
-- countSheep 0 = ""
-- countSheep x = show (x `mod` (x - 1)) ++ " sheep..." ++ countSheep (x `mod` (x + 2)) 


countSheep :: Int -> String
countSheep n
    | n == 0 = ""
    | otherwise = countSheep (n-1) ++ show n ++ " sheep..." 

-- findIndices (`elem` "aeiou") "Hello World!"
findNeedle :: [String] -> String
findNeedle xs = "found the needle at position " ++ show (fromJust (elemIndex "needle" xs))


-- rps :: String -> String -> String
-- rps p1 p2 | p1 == p2 = "Draw!"
--     | p1 == "scissors" && p2 == "paper"= "Player 1 won!"
--     | p1 == "paper" && p2 == "scissonr"= "Player 2 won!"
--     | p1 == "scissors" && p2 == "rock" = "Player 2 won!"
--     | p1 == "rock" && p2 == "scissors" = "Player 1 won!"
--     | p1 == "paper" && p2 == "rock" = "Player 1 won!"
--     | p1 == "rock" && p2 == "paper" = "Player 2 won!"


rps :: String -> String -> String
rps p1 p2
    | p1 == p2 = "Draw!"
    | p1 == "scissors" && p2 == "paper" = "Player 1 won!"
    | p1 == "paper" && p2 == "rock" = "Player 1 won!"
    | p1 == "rock" && p2 == "scissors" = "Player 1 won!"
    | otherwise = "Player 2 won!"


isInt x = x == fromInteger (round x)

isSquare :: Integral n => n -> Bool
isSquare n
    | n < 0 = False
    | otherwise = isInt (sqrt (fromIntegral n))



invert :: [Int] -> [Int]
invert [] = []
-- invert (x:xs) = (-x) : invert xs
-- invert (x:xs) = if x < 0 then (-x) : invert xs else x : invert xs
invert arr = [if x < 0 then (-x) else x | x <- arr]



betterThanAverage :: [Int] -> Int -> Bool
-- betterThanAverage (x:xs) y
--     | (x + betterThanAverage xs) `div` length x > y = True
--     | otherwise = False
-- betterThanAverage x y = if ((sum x + y) `div` ((length x) + 1)) <= y then False else True
betterThanAverage x y
    | ((sum x + y) `div` ((length x) + 1)) <= y = False
    | otherwise = True


--Sum all the numbers of a given array (cq.list), except the highest and the lowest element by value not by index
sumArray :: Maybe[Int] -> Int
sumArray Nothing = 0
sumArray (Just []) = 0
sumArray (Just [x]) = 0
sumArray (Just xs) = sum xs - maximum xs - minimum xs



printerError :: [Char] -> [Char]
printerError s = show (length [x | x <- s, x > 'm']) ++ "/" ++ show (length s)


endsWith :: String -> String -> Bool
endsWith x y = isSuffixOf y x


getInitials :: String -> String
getInitials x = intersperse '.' (map (toUpper . head) (words x))

countSheep' :: Int -> String
countSheep' 0 = ""
countSheep' x = show x ++ " sheep..." ++ countSheep' (x - 1)


main :: IO ()
main = do
    print $ getInitials "Yugal Khanal"
    print $ countSheep' 4
    -- print $ getInitials "Yugal Khanal"

    -- print $ findSmallestInteger list
    -- print $ squareSum list
    -- print $ fakeBin "1234567890"
    -- print $ countSheep 4
    -- print $ findNeedle ["hay", "junk", "hay", "hay", "moreJunk", "needle", "randomJunk"]
    -- print $ betterThanAverage [2, 3] 5
    -- print $ betterThanAverage [100, 40, 34, 57, 29, 72, 57, 88] 75
    -- print $ betterThanAverage [12, 23, 34, 45, 56, 67, 78, 89, 90] 69
    -- print $ betterThanAverage [12, 23, 34, 45, 56, 67, 78, 89, 90] 55
    -- print $ betterThanAverage [12, 23, 34, 45, 56, 67, 78, 89, 90] 54

    print $ endsWith "abc" "bc"