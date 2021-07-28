module Expr where
import Prelude hiding (EQ, GT, LT)

type Name = String

data Expr
  = Lit Lit
  | Var Name
  | BApp Op Expr Expr   -- e.g. BApp Add e1 e2
  | UApp Op Expr        -- e.g. Neg e
  | Quant Op [Name] Expr Expr
  | App Expr Expr
  --  | Op Op
  --  | Lam Name Expr
  deriving (Eq, Show)

data Op
  = EQ  -- relations
  | NEQ
  | LTE
  | GTE
  | LT
  | GT
    -- logic
  | Implies
  | Conj
  | Disj
  | Neg
    -- arithmetics
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Max
  | Min
  | Exp
  deriving (Eq, Show)

data Lit = Num Int | Bol Bool
  deriving (Eq, Show)

precedence :: Op -> Fixity
precedence EQ  = InfixL 5
precedence NEQ  = InfixL 4
precedence LTE  = InfixL 4
precedence GTE  = InfixL 4
precedence LT  = InfixL 4
precedence GT  = InfixL 4
precedence Implies  = InfixR 1
precedence Disj  = InfixL 2
precedence Conj  = InfixL 3
precedence Neg  = Prefix 6
precedence Add  = InfixL 7
precedence Sub  = InfixL 7
precedence Mul  = InfixL 8
precedence Div  = InfixL 8
precedence Mod  = InfixL 9
precedence Max  = Infix 10
precedence Min  = Infix 10
precedence Exp  = Infix 11

data Fixity = Infix Int | InfixR Int | InfixL Int | Prefix Int | Postfix Int
  deriving (Show, Eq)

-- convenient constructors

num :: Int -> Expr
num = Lit . Num

bol :: Bool -> Expr
bol = Lit . Bol

binOp :: Op -> Expr -> Expr -> Expr
binOp op e1 e2 = BApp op e1 e2 -- App (App (Op op) e1) e2

  -- SCM: not necessary now, but might be useful in the future.
add = binOp Add
mul = binOp Mul
lte = binOp LTE
sub = binOp Sub
eq = binOp EQ


free :: Expr -> [Name]
free (Lit n) = []
free (Var x) = [x]
-- SCM: finish the definition
{-
free (Op op) = []
free (App e1 e2) =  (free e1) ++ (free e2)
free (Quant e1 [n] e2 e3) = (filter (not.(== head notfree)) allvar) ++ (intersect (tail notfree) allvar)
   where
     notfree = intersect [n] (intersect (free e2) (free e3))
     allvar = (free e1) ++ [n] ++ (free e2)  ++ (free e3)
-}

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] ys = []
intersect (x:xs) ys = (filter (== x) ys) ++ (intersect xs ys)



{- some test example -}

test1 = ((num 3 `add` num 4) `add` num 5) `mul` (num 6)
test2 = Quant Add ["i"] range body
   where range = Var "m" `lte` Var "i" `lte` Var "n"
         body = App (Var "f") (Var "i")
test3 = ((Var "mo" `add` Var "q") `add` Var "e") `mul` (Var "v")

test4 = Var "a" `eq` num 3 `add` num 1
test5 = Var "b" `eq` num 3
test6 = Var "a" `eq` Var "a"`add` Var "c"
test7 = num 1 `eq` num 2
test8 = Var "b" `add` Var "a"

src01 = [("d",2)]

intersect1 :: Eq a => a -> [a] -> [a]
intersect1 x [] = []
intersect1 x (y:xs) | x == y = [x]
                    | otherwise = intersect1 x xs

intersect1' :: Eq a => [a] -> [a] -> [a]
intersect1' xs [] = []
intersect1' xs (y:ys) = (intersect1 y xs) ++ (intersect1' xs ys)

{-
0723
  1. finish intersect _ok
  2. redefine intersect1 and intersect using combinators, e.g.
      filter, map, foldr, concat ....ok
  3. free (Quant e1 [n] e2 e3)... ok
  4. test4,6 !!!

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

thrid :: (a,b,c)->c
thrid (a,b,c) = c
-}


{-
eval :: [(Name, Int)] -> Expr -> Int
3 + 5 -> 8
x + 5 -> ???
eval [("y",9)] (x ^ 3 + 5)
-}

-- eval :: [(Name, Int)] -> Expr -> Int
--eval xs e1 = 0

--getanswer () =
getanswer (Lit n) = n
-- :t num 3 :: Expr
-- :t Num 3 :: Lit
-- not int

--getanswer (Var x) = [x]

--eval [(x,y)] e1 =


-- eval :: [(Name, Int)] -> Expr -> Int
-- eval [] (Lit n) = n
-- eval [(y,n)] (Var x) = filter (\x \y -> (x == y)) . (\x -> y
-- eval [] (Op op) = [op]
--eval [] (App e1 e2) =  (eval e1) ++ (eval e2)

{-
0723
compare2 :: Eq a => ([a], [a], [a]) -> ([a],[a],[a])
compare2 (x,[],z) =  (x,[],z)
compare2 ([],y:ys,[]) =  ([],ys,[])
compare2 ([],y,z) =  ([],y,z)
compare2 (x:xs,y:ys,zs) = if (x == y)
    then compare2 (xs, y:ys, x:zs)
    else compare2 (xs, y:ys, zs)
-}
