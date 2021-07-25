module Expr where
import Prelude hiding (EQ, GT, LT)

type Name = String

data Expr
  = Lit Lit
  | Var Name
  | Op Op
  | App Expr Expr
--  | Lam Name Expr
  | Quant Expr [Name] Expr Expr
  deriving (Eq, Show)
--lit 字元
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
--整數多項式
bol :: Bool -> Expr
bol = Lit . Bol

binOp :: Op -> Expr -> Expr -> Expr
binOp op e1 e2 = App (App (Op op) e1) e2

add = binOp Add
mul = binOp Mul
lte = binOp LTE
sub = binOp Sub
eq = binOp EQ


test1 = ((num 3 `add` num 4) `add` num 5) `mul` (num 6)
test2 = Quant (Op Add) ["i"] range body
   where range = Var "m" `lte` Var "i" `lte` Var "n"
         body = App (Var "f") (Var "i")
test3 = ((Var "mo" `add` Var "q") `add` Var "e") `mul` (Var "v")

--val :: [(Name, Int)] -> Expr -> Int


src01 = [("d",2)]

test0 = Var "a" `eq` num 3 `add` num 1
test00 = Var "b" `eq` num 3

test002 = Var "a" `eq` Var "a"`add` Var "c"
test003 = num 1 `eq` num 2
test004 = Var "b" `add` Var "a"

free :: Expr -> [Name]
free (Lit n) = []
free (Var x) = [x]
free (Op op) = []
free (App e1 e2) =  (free e1) ++ (free e2)
{-

free (Quant e1 [n] e2 e3) =
  if thrid(xs1) == thrid(xs2)
  then
    where
      xs1 = thrid (compare2 ((free e1),(free e2),[]))
      xs2 = thrid (compare2 ((free e2),(free e1),[]))
      ys1 = thrid (compare2 ((free e2),(free e3),[]))
      ys2 = thrid (compare2 ((free e3),(free e2),[]))
      zs = compare2 (xs,ys,zs)
  --   ++ [n] ++ ["e2"]++(free e2) ++ ["e3"]++ (free e3)
-}
{-
findfreevar :: [a] -> [Name] -> [Name] -> ([a],[Name],[Name])
findfreevar [] (x:xs) (y:ys) =
  if (x == y)
    then findfreevar([x], xs, y:ys)
    else findfreevar([ ], xs, y:ys)
-}
{-
compare :: [Name] -> [Name] -> [Name] -> [Name]
compare [] [] = []
compare x:xs [] = x:xs
compare [] y:ys = y:ys
compare x:xs y:ys  = if (x == y) then x:[xs][ys]
-}

compare2 :: ([Name],[Name],[Name]) -> ([Name],[Name],[Name])
-- compare2 ([],[],[]) =  ([],[],[])
-- compare2 (x,[],[]) =  (x,[],[])
compare2 (x,[],z) =  (x,[],z)
compare2 ([],y:ys,[]) =  ([],ys,[])
compare2 ([],y,z) =  ([],y,z)
-- compare2 (x:xs,y:ys,[]) = if (x == y)
--     then compare2 (xs, y:ys, [x])
--     else compare2 (xs, y:ys, [])
compare2 (x:xs,y:ys,zs) = if (x == y)
    then compare2 (xs, y:ys, x:zs)
    else compare2 (xs, y:ys, zs)

-- input1 input2 memorize"input2", output



--comaparecycle ()=



swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

thrid :: (a,b,c)->c
thrid (a,b,c) = c

forth :: (a,b,c,d)->d
forth (a,b,c,d) = d



-- head (compare2 x:xs)

{-
eval :: [(Name, Int)] -> Expr -> Int
3 + 5 -> 8
x + 5 -> ???
eval [("y",9)] (x ^ 3 + 5)
-}


--eval :: [(Name, Int)] -> Expr -> Int
--eval (n,x) e =

-- eval [] e = 0
-- eval [(n,x)] e =

--free :: Expr -> [Name]
