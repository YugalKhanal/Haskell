-- list :: [Int]
list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

emptyList :: [Int]
emptyList = []

multiply :: Int -> Int -> Int
multiply x y = x * y

multiply' :: Int -> Int
multiply' x = x * x

-- ex
orB :: Bool -> Bool -> Bool
orB True _ = True
orB _ _ = False

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

third :: [a] -> a
third l = head $ tail $ tail l

third' :: [a] -> a
third' l = l !! 2

third'' :: [a] -> a
third'' (_ : _ : x : _) = x

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

safeTail :: [a] -> [a]
safeTail l = if isEmpty l then [] else tail l

safeTail' :: [a] -> [a]
safeTail' l
  | isEmpty l = []
  | otherwise = tail l

safeTail'' :: [a] -> [a]
safeTail'' l = case l of
  [] -> []
  _ -> tail l

safeTail''' :: [a] -> [a]
safeTail''' [] = []
safeTail''' (_ : xs) = xs

{--
instance (Ord a) => Ord [a] where
    compare [] [] = EQ
    compare [] (_ : _) = LT
    compare (_ : _) [] = GT
    compare (x : xs) (y : ys) = case compare x y of
        EQ -> compare xs ys
        other -> other
--}

main :: IO ()
main = do
  print $ safeTail''' emptyList
  print $ safeTail'' emptyList
  print $ safeTail' emptyList
  print $ safeTail emptyList
  print $ third'' list
  print $ third list
  print $ third' list
  print $ swap (1, 2)
  print $ multiply 2 3
  print $ multiply' 2
  print $ length list
  print $ reverse list
  print $ tail list
  print $ head list
  print $ take 5 list
  print $ drop 2 list
  print $ takeWhile (< 5) list -- takeWhile returns the elements until the condition is met// takeWhile, applied to a predicate p and a list xs, returns the longest prefix (possibly empty) of xs of elements that satisfy p.
  print $ dropWhile (< 5) list -- dropWhile, applied to a predicate p and a list xs, returns the suffix remaining after takeWhile p xs.
  print $ filter even list
  print $ filter odd list
  print $ map (* 15) list
  print $ all even list -- all, applied to a predicate p and a list xs, determines if all elements of xs satisfy p.
  print $ any even list -- any, applied to a predicate p and a list xs, determines if any element of xs satisfies p.
  print $ orB True True