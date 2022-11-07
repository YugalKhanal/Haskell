-- (ghc --make week3.hs)

exList :: [Int]
exList = [1,2,3,4,5]

hello = "Hello World"
doubleIt :: Int -> Int
doubleIt x = x * 2
-- map doubleIt [1,2,3,4,5]

removeLast :: [a] -> [a]
removeLast l = reverse $ tail $ reverse l

keepOdds :: [Int] -> [Int]
keepOdds l = filter odd l

isBigString' :: String -> String
isBigString' s | length s > 14 = "It is a big string"
    | length s < 3 = "It is a small string"
    | otherwise = "It is a medium string"

listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

isEmpty' :: [a] -> Bool
isEmpty' l = case l of
    [] -> True
    _ -> False


and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False


evenEls :: [a] -> [a]
evenEls [] = []
evenEls (_:[])  = []
evenEls (_:y:xs) = y : (evenEls xs)

oddEls :: [a] -> [a]
oddEls [] = []
oddEls (x:[]) = x:[]
oddEls (x:_:xs) = x : (oddEls xs)

isBigString :: String -> String
isBigString s = if length s > 4 then "It is a big string" else "It is a small string"


-- Lambda expressions
double :: Int -> Int
double x = x + x

double' :: (Int -> Int)
double' = \x -> x + x

doubleAll :: [Int] -> [Int]
doubleAll l = map double' l

doubleAll' :: [Int] -> [Int]
doubleAll' l = map (\x -> x + x) l

doubleAll'' :: [Int] -> [String]
doubleAll'' l = map (\x -> if (x > 3) then "Big number" else "Small number") l

append :: [a] -> [a] -> [a]
append x y = x ++ y

main :: IO()
main = do
    print $ doubleIt 5
    print $ removeLast exList
    print $ keepOdds exList
    print $ isBigString "Hello world!"
    print $ isBigString' "Wowdf"
    print $ listLength exList
    print $ doubleAll exList
    print $ doubleAll' exList
    print "Hello World"
    print $ doubleAll'' exList
    print $ doubleAll'' exList