
-- Ilan Weiss
-- 302634654


-- 1.

isPalindrome :: String -> Bool

isPalindrome x = x == reverse x

-- 2.

isPrefix :: String -> String -> Bool

isPrefix [][] = True
isPrefix [](ys) = True
isPrefix (xs)[] = False
isPrefix (x:xs)(y:ys) = (x == y)&&(isPrefix xs ys) 

--3a.

squareList :: Int -> [Int]

squareList n = if (n == 0) 
               then [0]
               else n^2 : squareList (n-1)
--3b.

listSquare :: Int -> [Int]

listSquare n = reverse(squareList n)

--4.

fact :: Integer -> Integer

fact 0 = 1
fact n = n*fact(n-1)

--5a.

toDigits' :: Integer -> [Integer]
toDigits' n = if (n > 0)
             then (n `mod` 10):toDigits'(n `div` 10)
             else []

toDigits :: Integer -> [Integer]
toDigits n = reverse(toDigits' n)

--5b.

doubleEveryOther :: [Integer]->[Integer]
doubleEveryOther xs = if ((length xs)`mod`2==0)
                      then buildEven xs
                      else buildOdd xs


buildEven :: [Integer] -> [Integer]

buildEven [] = [] 
buildEven (x:[]) = [x*2]
buildEven (x:x2:xs) = [x*2,x2]++buildEven (xs)


buildOdd :: [Integer] -> [Integer]

buildOdd [] = []
buildOdd (x:[]) = [x]
buildOdd (x:x2:xs) =[x,x2*2]++buildOdd(xs)

--5c.

sumDigits :: [Integer] -> Integer

sumDigits [] = 0
sumDigits (x:xs) = sum(toDigits x) + sumDigits(xs)  

--5d.

validate :: Integer -> Bool
validate n =((sumDigits (doubleEveryOther (toDigits (n)))) `mod` 10) == 0