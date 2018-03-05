doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = (if x > 100 then x else x*2)
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
a = head [1,2,3,4]
b = tail [1,2,3,4]
c = last [1,2,3,4]
d = init [1,2,3,4]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]

addFour :: Int -> Int -> Int -> Int -> Int
addFour i j k l = i + j + k + l


circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r
