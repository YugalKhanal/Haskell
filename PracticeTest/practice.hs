{- Question 1 -}
checksum :: Integral a => [a] -> Bool
checksum [] = False
checksum x = length x == 8 && sum x `mod` 11 == 0

{- Question 2 -}
golfScorer :: Integer -> Integer -> Integer
golfScorer par strokes
    | strokes == 1 = 5
    | par - strokes >= 2 = 4
    | par - strokes == 1 = 3
    | par == strokes = 2
    | par - strokes == -1 = 1
    | par - strokes <= -1 = 0

{- Question 3 -}
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]


--List the first n numbers starting from 1 which are divisible by all the numbers from 2 to 12.
highlyDivisible :: Int -> [Int]
factorList = [2 .. 12]
highlyDivisible n = take n [x | x <- [1 .. ], all (== 0) (map (x `mod`) factorList)]



-- List the largest odd factor of i for all numbers i in the range [1..n].
largestOddFactor :: Int -> [Int]
largestOddFactor n = drop (length (filter odd [x | x <- [1 .. n], n `mod` x == 0]) - 1) $ filter odd [x | x <- [1 .. n], n `mod` x == 0]


{- Question 4 -}
equals :: (Enum a, Bounded a, Eq b) => (a -> b) -> (a -> b) -> Bool
equals = undefined

{- Question 5 -}

babylonianPalindromes :: [Integer]
babylonianPalindromes = undefined

main :: IO ()
main = do
    print $ largestOddFactor 60


