
-- Ilan Weiss
-- 302634654


-- 1.

type Variable = String
type Value = Float
type Dictionary = [(Variable,Value)]
type EvalError = [Variable]
type EvalResult = Either EvalError Value

data Expr =
  Const Value
 | Add Expr Expr
 | Mul Expr Expr
 | Sub Expr Expr
 | Div Expr Expr
 | Var Variable
 deriving(Show, Eq)


--a.
display :: Expr->String

display (Const n) = show n
display (Add a b) = "(" ++ (display a) ++ "+" ++ (display b) ++ ")"
display (Mul a b) = "(" ++ (display a) ++ "*" ++ (display b) ++ ")"
display (Sub a b) = "(" ++ (display a) ++ "-" ++ (display b) ++ ")"
display (Div a b) = "(" ++ (display a) ++ "/" ++ (display b) ++ ")"
display (Var v) = v

--b


eval :: Dictionary -> Expr -> EvalResult

eval _ (Const d) = Right d
eval xs (Add a b) = app xs (+) a b
eval xs (Mul a b) = app xs (*) a b
eval xs (Sub a b) = app xs (-) a b
eval xs (Div a b) = app xs (/) a b
eval xs (Var v) = search xs v


search :: Dictionary -> Variable ->EvalResult

search [] v = Left [v]
search (x:xs) v | fst x == v  = Right (snd x)
                | otherwise   = search xs v

app :: Dictionary -> (Value -> Value -> Value) -> Expr -> Expr -> EvalResult

app xs f a b =
         case eval xs a of
                  Left varA -> case eval xs b of
                        Left varB -> Left (varA ++ varB)
                        Right _ -> Left varA
                  Right valA -> case eval xs b of
                        Left varB -> Left varB
                        Right valB -> Right (f valA valB)

--2a.

data Tree a b = Leaf b
               |Node a (Tree a b) (Tree a b)
               deriving (Show,Eq)

reverseTree :: Tree a b->Tree a b

reverseTree  (Leaf b) = Leaf b
reverseTree (Node a t1 t2) = Node a (reverseTree t2) (reverseTree t1)

--2b.

isSubtree :: Tree Int Char -> Tree Int Char -> Bool

isSubtree (Leaf a) (Leaf b) = (a == b)
isSubtree (Node a t1 t2) (Leaf b) = False
isSubtree (Leaf a) (Node b t1 t2) = (isSubtree (Leaf a) (t1))
                                || (isSubtree (Leaf a) (t2))
isSubtree (Node a x y) (Node b t1 t2) = if (a==b)
                                       then (isSubtree (x ) (t1)) && (isSubtree (y) (t2))
                                       else (isSubtree (Node a x y) (t1))
                                         || (isSubtree (Node a x y) (t2))

--3a.

data MTree a = MTree a [MTree a] deriving (Show,Eq)

sumMTree :: MTree Int -> Int

sumMTree (MTree a [])  = a
sumMTree (MTree a b)  = a + sum(map (sumMTree) b)

--3b.

grow :: MTree a -> MTree a
grow (MTree a b) = helpGrower (MTree a b) (MTree a b)

helpGrower :: MTree a -> MTree a -> MTree a

helpGrower orginal (MTree a []) = orginal
helpGrower orginal (MTree a b) = MTree a
                              (map (\new -> helpGrower orginal new) b)
