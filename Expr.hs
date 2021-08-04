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
sub = binOp Sub
mod' = binOp Mod
div' = binOp Div

eq = binOp EQ
neq = binOp NEQ
lte = binOp LTE
lt = binOp LT
gt = binOp GT


free :: Expr -> [Name]
free (Lit n) = []
free (Var x) = [x]
free (BApp op e1 e2) = free e1 ++ free e2
-- e.g. BApp Add e1 e2
free (UApp op e) = free e
-- e.g. Neg e
free (Quant op xs e1 e2) = subtraction ((free e1 ++ free e2), xs)
free (App e1 e2) = free e1 ++ free e2

-- SCM: finish the definition

subtraction1 :: Eq a => [a] -> a -> [a]
subtraction1 [] y = []
subtraction1 (x:xs) y | (x == y)  = subtraction1 xs y
                      | otherwise = x : subtraction1 xs y

subtraction :: Eq a => ([a],[a]) -> [a]
subtraction ([], []) = []
subtraction (xs, []) = xs
subtraction (xs, y:ys) = subtraction (filter (not . (==y)) xs, ys)
                         -- was: subtraction1 xs y



{- some test example -}
test1 = ((num 3 `add` num 4) `add` num 5) `mul` (num 6)
test2 = Quant Add ["i"] range body
   where range = Var "m" `lte` Var "i" `lte` Var "n"
         body = App (Var "f") (Var "i")
test3 = ((Var "a" `add` Var "b") `add` Var "a") `mul` (Var "b")

test4 = Var "a" `eq` (num 3 `add` num 1)
test5 = Var "b"

test6 = Quant Add ["a","i"] range body
   where range = Var "a" `lte` Var "i" `lte` Var "b"
         body = App (Var "b") (Var "i")

test7 = num 1
test8 = Var "a" `neq` Var "b"
test9 = Var "a" `div'` Var "b"
test0 = Var "a" `add` (UApp Neg (Var "b"))
testlist = [("a",-2),("b",5)]


eval1 :: [(Name, Int)] -> Expr -> Maybe Int
eval1 _  (Lit (Num n)) = Just n
eval1 [] (Lit (Bol b)) = Nothing
eval1 [] (Var y) = Nothing
eval1 xs (Var y) = case lookup y xs of
                          Just n -> Just n
                          otherwise -> Nothing

eval1 xs (UApp Neg e1) = case eval1 xs e1 of
                            Just n -> Just (-n)
                            otherwise -> Nothing

eval1 xs (BApp op e1 e2) = case eval1 xs e1  of
                            Just n -> case eval1 xs e2 of
                                          Just m -> case op of
                                                    Add -> Just (m + n)
                                                    Sub -> Just (m - n)
                                                    Mul -> Just (m * n)
                                                    Div -> Just (div m n)
                                                    Mod -> Just (mod m n)
                                                    otherwise -> Nothing
                            otherwise -> Nothing

-- Exercise: extend eval
data Val = VNum Int | VBol Bool
          deriving (Eq, Show)

eval :: [(Name, Int)] -> Expr -> Maybe Val
eval _ (Lit (Num n)) = Just (VNum n)
eval _ (Lit (Bol b)) = Just (VBol b)
eval [] (Var y) = Nothing
eval xs (Var y) = case lookup y xs of
                          Just n -> Just (VNum n)
                          otherwise -> Nothing

eval xs (UApp Neg e1) = case eval xs e1 of
                            Just (VNum n)  -> Just (VNum (-n))
                            otherwise -> Nothing

eval xs (BApp op e1 e2) = case eval xs e1 of
                            Just (VNum n) -> case eval xs e2 of
                                          Just (VNum m) -> case op of
                                                    Add -> Just (VNum (m + n))
                                                    Sub -> Just (VNum (m - n))
                                                    Mul -> Just (VNum (m * n))
                                                    Div -> Just (VNum (div m n))
                                                    Mod -> Just (VNum (mod m n))
                                                    EQ  -> Just (VBol (m == n))
                                                    NEQ -> Just (VBol (m /= n))
                                                    LTE -> Just (VBol (m <= n))
                                                    GTE -> Just (VBol (m >= n))
                                                    LT  -> Just (VBol (m < n))
                                                    GT  -> Just (VBol (m > n))
                                                    otherwise -> Nothing
                                          otherwise -> Nothing
                            otherwise -> Nothing -- something intersting 0802

-- eval xs (Quant op [ys] e1 e2) = case eval xs e1 of
--                             Just (VNum n)  -> Just (VNum (-n))
--                             otherwise -> Nothing
{-

* There is a built-in function lookup, having this type:

   lookup :: Eq a => a -> [(a, b)] -> Maybe b

* Also, the Maybe datatype can be defined by:

   data Maybe a = Just a | Nothing

* If f has type Int -> Maybe Char, to use it you may have to

    case f 3 of
      Just c -> ... c ...  -- c has type Char
      Nothing -> Nothing

* Exercise: Redefine eval. Let it have type:

   eval :: [(Name, Int)] -> Expr -> Maybe Int

   eval [("a", 3), ("b", 4)]  (.... a b ... c ...) = Nothing
   eval [("a", 3), ("b", 4)] (a + b + 3) = Just 10

   -- In the (Var x) case, use lookup above.

* Exercise: define your own lookup. Use a different name,
  e.g. lookUp.

* Exercise: extend eval

  data Val = VNum Int | VBol Bool

  eval :: [(Name, Int)] -> Expr -> Maybe Val

  eval [("a", 3), ("b", 4)] (a + b + 3) = Just (VNum 10)
  eval [("a", 3), ("b", 4)] (a == (b - 2)) = Just (VBol False)
  eval [("a", 3), ("b", 4)] (a + c) = Nothing
  eval [] (Lit (Num 3)) = Just (VNum 3)
  eval [("a", 3), ("b", 4)] (a + (b == a)) = Nothing

---

* Declarations in Haskell

  f x = ...
    where y = x + 1
          z = (x == y - 1)

  g _ = ...x ...
     where x = x + 1
           xs = 1 : xs

  let xs = 1 : xs
  take 20 xs

* In Python or C....
   y = x
   y = x + 1
   x = x + 1

  { x + 1 = 3 } x := x + 1  { x = 3 }

-}



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

{-
0723
  1. finish intersect _ok
  2. redefine intersect1 and intersect using combinators, e.g.
      filter, map, foldr, concat ....ok
  3. free (Quant e1 [n] e2 e3)... ok

0731 ok
eval :: [(Name, Int)] -> Expr -> Int
3 + 5 -> 8
x + 5 -> ???
eval [("y",9)] (x ^ 3 + 5)
-}

{-
free (Op op) = []
free (App e1 e2) =  (free e1) ++ (free e2)
free (Quant e1 [n] e2 e3) = (filter (not.(== head notfree)) allvar) ++ (intersect (tail notfree) allvar)
   where
     notfree = intersect [n] (intersect (free e2) (free e3))
     allvar = (free e1) ++ [n] ++ (free e2)  ++ (free e3)
-}

--eval [] (BApp Add (Lit (Num 3)) (Lit (Num 4)))
{-
eval :: [(Name, Int)] -> Expr -> Int
eval _     (Lit (Num n)) = n
-- eval [] (Lit (Bol b)) = 0
-- eval [] (Var y) = 0
eval (x:xs) (Var y) | (y == fst x) = snd x
                    | otherwise = eval xs (Var y)
eval xs (BApp Add e1 e2) = (eval xs e1) + (eval xs e2)
eval xs (BApp Sub e1 e2) = (eval xs e1) - (eval xs e2)
eval xs (BApp Mul e1 e2) = (eval xs e1) * (eval xs e2)
-- eval (x:xs) (BApp Div e1 e2) = (eval (x:xs) e1) / (eval (x:xs) e2)
-- eval [] (BApp lte e1 e2) = (eval [] e1) + (eval [] e2)
-- eval [] (BApp eq e1 e2) = (eval [] e1) + (eval [] e2)
eval (x:xs) (UApp Neg e1) = - (eval (x:xs) e1)
--eval (x:xs) (App e1 e2) = App (eval (x:xs) e1) (eval (x:xs) e2)

lookUp ::

-}
