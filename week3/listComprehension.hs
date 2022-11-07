lengthh :: [a] -> Int
lengthh xs = sum [1 | _ <- xs]

list :: [Int]
list = [1 .. 5]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], prime x]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

pyths :: Int -> [(Int, Int, Int)]
pyths x = [(a, b, c) | a <- xs, b <- xs, c <- xs, a ^ 2 + b ^ 2 == c ^ 2]
  where
    xs = [1 .. x]

factors' :: Int -> [Int]
factors' n = [x | x <- [1 .. n - 1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], sum (factors' x) == n]

dotProd :: [Int] -> [Int] -> Int
dotProd x y = sum [i * j | (i, j) <- zip x y]

list0 = [1, 2]

list1 = [3, 4]

main :: IO ()
main = do
  print $ dotProd list0 list1
  print $ perfects 500
  print $ factors' 6
  print "hello"
  print (lengthh list)
  print (primes 10)
  print (pyths 5)
  print $ sum list