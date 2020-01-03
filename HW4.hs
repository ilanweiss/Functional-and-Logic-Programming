
-- Ilan Weiss
-- 302634654
--------------------------------------------------------------------------------
data Bit = O | I
  deriving (Eq, Show)
data HuffmanTree = Leaf Char Int | Node Int HuffmanTree HuffmanTree
  deriving (Eq, Show)
type Code = [Bit]
type FreqTable = [(Char, Int)]
type Dictionary = [(Char, Code)]
type Encoder = Char -> Code
--------------------------------------------------------------------------------
--1a.
insert :: Char -> FreqTable -> FreqTable

insert x [] = [(x,1)]
insert x ((x',val):ls) = if x == x'
                        then [(x',val + 1)] ++ ls
                        else [(x',val)] ++ insert x ls
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--1b.

count :: String -> FreqTable

count (x:[]) = insert x []
count (x:xs) = insert x (count xs)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--2.

initLeaves :: FreqTable -> [HuffmanTree]

initLeaves ((x,val):[]) = [Leaf x  val]
initLeaves ((x,val):xs) = [Leaf x  val] ++ initLeaves xs
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--3.

buildTree :: [HuffmanTree] -> HuffmanTree

buildTree x = addTree (sortTree x)
--------------------------------------------------------------------------------

sortTree :: [HuffmanTree] -> [HuffmanTree]

sortTree [] = []
sortTree (x:xs) = sortTree smaller ++ [x] ++ sortTree larger
                where
                     smaller = [a | a <- xs, getVal(a) < getVal(x)]
                     larger = [b | b <- xs,  getVal(b) >= getVal(x)]
--------------------------------------------------------------------------------

getVal:: HuffmanTree -> Int

getVal (Leaf x val) = val
getVal (Node val x y) = val
--------------------------------------------------------------------------------

addTree :: [HuffmanTree] -> HuffmanTree

addTree (x:[]) = x
addTree (x:y:[]) = if (getVal(x) >= getVal(y))
                   then (Node (getVal(x) + getVal(y)) x y)
                   else (Node (getVal(x) + getVal(y)) y x)
addTree (x:y:ls) = addTree (sortTree ((addTree (x:y:[])):ls))
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--4.

createDictionary :: HuffmanTree -> Dictionary

createDictionary (Leaf x val) = [(x,[])]
createDictionary (Node val x y) =
                                 helper O (createDictionary (x)) ++
                                 helper I (createDictionary (y))
--------------------------------------------------------------------------------
helper :: Bit -> Dictionary -> Dictionary

helper b [] = []
helper b ((x,d):ls) = (x,b:d):helper b ls
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--5.

createEncoder :: Dictionary -> Encoder

createEncoder [] x' = []
createEncoder (x:xs) x' = if ((fst x)== x')
                          then snd x
                          else createEncoder xs x'
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--6.
encode :: String -> (HuffmanTree, Code)

encode str = (encodeTree str, encodeCode str (createDictionary (encodeTree str)))
--------------------------------------------------------------------------------
encodeTree:: String -> HuffmanTree

encodeTree str = buildTree (initLeaves (count str))
--------------------------------------------------------------------------------
encodeCode :: String -> Dictionary -> Code

encodeCode [] _ = []
encodeCode (s:str) dict = createEncoder dict s ++ encodeCode str dict
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--7.
decode :: HuffmanTree -> Code -> String

decode tree c = decode' tree tree c
--------------------------------------------------------------------------------
decode' :: HuffmanTree -> HuffmanTree -> Code -> String

decode' orignal (Leaf x val)  [] = [x]
decode' orignal (Leaf x val) cs = [x] ++ decode' orignal orignal cs
decode' orignal (Node val x y) (c:cs) = if (c == O)
                                      then decode' orignal x cs
                                      else decode' orignal y cs
