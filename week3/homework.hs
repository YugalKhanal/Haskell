data Classification = Low | Medium | High | SuperHigh deriving (Show)

gasUsage :: (Fractional a, Ord a) => a -> Classification
gasUsage g
    | g < 3 = Low
    | 3 <= g || g < 5 = Medium
    | 5 <= g || g < 7 = High
    | g >= 7 = SuperHigh

luhnDouble :: Int -> Int
luhnDouble x = if x * 2 > 9 then x * 2 - 9 else x * 2

luhnDouble' :: Int -> Int
luhnDouble' x
        | x * 2 > 9 = x * 2 -9
        | otherwise = x * 2

-- The Luhn algorithm is used to check bank card numbers for simple errors such as mistyping a digit, and proceeds as follows:

-- consider each digit as a separate number;
-- moving left, double every other number from the second last;
-- subtract 9 from each number that is now greater than 9;
-- add all the resulting numbers together;
-- if the total is divisible by 10, the card number is valid.


luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (luhnDouble w + x + luhnDouble y + z) `mod` 10 == 0