
-- Ilan Weiss
-- 302634654

import Data.List
import qualified Data.Map as Map

-- 1a.

replaceElement :: (Int,a)->[a]->[a]

replaceElement (i,x) xs = if i < 0 || i >= length xs
                        then xs
                        else replaceElement' (i,x) xs

replaceElement' :: (Int,a)->[a]->[a]
replaceElement' (i,x) xs = take i xs ++ (x:drop (i+1) xs)

-- 1b.

replaceElements :: [(Int,a)] -> [a] -> [a]

replaceElements [] xs = xs
replaceElements ((i,x):is) xs = replaceElements is
                                (replaceElement (i,x) xs)

--2a.

addItem :: (String,a)->[(String,a)]->[(String,a)]

addItem (str,x) xs = ((str,x):xs)


--2b. 

subsetByKey :: String->[(String,a)]->[a]

subsetByKey str [] = [] 
subsetByKey str ((stri,x):xs) = if (str == stri)
                                 then x:subsetByKey str xs 
                                 else subsetByKey str xs

--2c. 

subsetByKeys :: [String]->[(String,a)]->[a]

subsetByKeys [] (x:xs) = []
subsetByKeys (str:strs) (x:xs) = subsetByKey str (x:xs) ++               
                                 subsetByKeys strs (x:xs) 

--2d.

getKeys :: [(String,a)]->[String]

getKeys [] = []
getKeys (x:xs) = uniq (map fst (x:xs))

uniq :: [String] -> [String]

uniq [] = []
uniq (x:xs) = x: uniq (filter (/=x) xs)

--2e.

groupByKeys :: [(String,a)]->[(String,[a])]

groupByKeys [] = []
groupByKeys (xs) = pairing(getKeys (xs)) xs


pairing :: [String] -> [(String,a)]-> [(String,[a])]

pairing [] _ = []
pairing (str:strs) (xs) = (str,subsetByKey str xs) 
                         : pairing strs xs


--3a.

createMatrix :: Int -> Int -> [a] -> [[a]]

createMatrix m n [] = []
createMatrix m n xs = take n xs:createMatrix m n (drop n xs)

--3b.

getElementInCell :: Int -> Int -> [[a]] -> a

getElementInCell m n xs = take 1 (drop (m) xs) !! 0 !! n


--3c.

appendH :: [[a]] -> [[a]] -> [[a]]

appendH [] [] = []
appendH (x:xs) (y:ys) = [x ++ y] ++ appendH xs ys


--3d.

appendV :: [[a]] -> [[a]] -> [[a]]

appendV [] [] = []
appendV xs ys = xs ++ ys

--3e.

addMatrices :: [[Int]] -> [[Int]] -> [[Int]]

addMatrices [] [] = []
addMatrices (x:xs) (y:ys) = [addRows x y] ++ addMatrices xs ys

addRows :: [Int]->[Int]->[Int]

addRows [] [] = []
addRows (x:xs) (y:ys) = (x+y) : addRows xs ys








