
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine :: (Num a) => a -> a -> a
multTwoWithNine = multThree 9

multWithEighteen :: (Num a) => a -> a
multWithEighteen = multTwoWithNine 2

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanumeric :: Char -> Bool
isUpperAlphanumeric = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

-- Equivalent:
--flip' :: (a -> b -> c) -> (b -> a -> c)
--flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

-- Rewritten quicksort', using filter.
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
    let smallerSorted = quicksort' (filter' (<=x) xs)
        biggerSorted = quicksort' (filter' (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted


-- This is really cool because the caller (head) of filter is asking only for
-- the first element of an infinite list.
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n
    | even n = n:collatzChain (n `div` 2)
    | odd n = n:collatzChain (n*3 + 1)

numLongCollatzChains :: Int
numLongCollatzChains = length (filter isLong (map collatzChain [1..100]))
    where isLong xs = length xs > 15

-- Using a lambda:
numLongCollatzChains' :: Int
numLongCollatzChains' = length (filter (\xs -> length xs > 15) (map collatzChain [1..100]))

-- Add three:
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

-- Pointless, but using lambdas:
addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

-- Flip, using lambdas:
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

-- Sum, using folds:
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- Sum, reduced by relying on currying:
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

-- Elem, using a fold:
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- Map, using folds:
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs


-- Maximum, using folds:
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

-- Reverse, using folds:
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

-- Product, using folds:
product' :: (Num a) => [a] -> a
product' = foldr1 (*)

-- Filter, using folds:
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc ) []

-- Head, using folds:
head' :: [a] -> a
head' = foldr1 (\x _ -> x)

-- Last, using folds:
last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- Number of square roots it takes to get to 1000 when adding them up.
sqrtSums :: Int
sqrtSums = length ( takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- "Point-free style : before & after"
fn x = ceiling (negate (tan (cos (max 50 x))))
fn' = ceiling . negate . tan . cos . max 50



-- Sum of all odd squares smaller than 10,000.
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- Rewritten with function composition:
oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- But that was less readable in this case! Rewritten for clarity:
oddSquareSum'' :: Integer
oddSquareSum'' = 
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit

























































