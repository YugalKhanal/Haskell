and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = [x] ++ replicate' (n-1) x

(!!!) :: [a] -> Int -> a
(!!!) [] _ = undefined
(!!!) (x:xs) n | n == 0 = x
        | otherwise = (!!!) xs (n-1)


main :: IO ()
main = do
    print $ replicate' 9 9
    print $ (!!!) [1,2,3,4,5,6,7,8,9,10] 7
