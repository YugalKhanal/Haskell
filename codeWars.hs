and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- replicate' :: Int -> a -> [a]
-- replicate' 0 _ = []
-- replicate' n x = 

inlove :: Int -> Int -> Bool
inlove a b = odd $ a + b

-- digitize :: Int -> [Int]
-- digitize 0 = [0]
-- digitize x = 

-- 35231 => [1,3,2,5,3]

digitize :: Int -> [Int]
digitize 0 = [0]
digitize x = reverse $ digitize (x `div` 10) ++ [x `mod` 10]


countSheep :: [Bool] -> Int
countSheep x = length [y | y <- x, y == True]


arr = [True,  True,  True,  False,
True,  True,  True,  True ,
True,  False, True,  False,
True,  False, False, True ,
True,  True,  True,  True ,
False, False, True,  True]






main :: IO ()
main = do
    print $ inlove 2 5
    print $ digitize 35231
    print $ countSheep arr