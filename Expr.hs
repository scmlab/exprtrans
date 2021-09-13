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

-- calculate
cal1 :: Op -> Int -> Maybe Int
cal1 Neg m = Just (-m)

cal2 :: Op -> Int -> Int -> Maybe Int
cal2 Add m n = Just (m + n)
cal2 Sub m n = Just (m - n)
cal2 Mul m n = Just (m * n)
cal2 Mod m n = case n of
                  0 -> Nothing
                  otherwise -> Just (mod m n)
cal2 Div m n = case n of
                  0 -> Nothing
                  otherwise -> Just (div m n)

cal0 :: Op -> Val -> Maybe Val
cal0 Neg (VNum m) = Just (VNum (-m))

cal :: Op -> Val -> Val -> Maybe Val
cal Add (VNum m) (VNum n) = Just (VNum (m + n))
cal Sub (VNum m) (VNum n) = Just (VNum (m - n))
cal Mul (VNum m) (VNum n) = Just (VNum (m * n))
cal Mod (VNum m) (VNum n) = case n of
                              0 -> Nothing
                              otherwise -> Just (VNum (mod m n))
cal Div (VNum m) (VNum n) = case n of
                              0 -> Nothing
                              otherwise -> Just (VNum (div m n))
cal EQ  (VNum m) (VNum n) = Just (VBol (m == n))
cal NEQ (VNum m) (VNum n) = Just (VBol (m /= n))
cal LTE (VNum m) (VNum n) = Just (VBol (m <= n))
cal GTE (VNum m) (VNum n) = Just (VBol (m >= n))
cal LT  (VNum m) (VNum n) = Just (VBol (m < n))
cal GT  (VNum m) (VNum n) = Just (VBol (m > n))



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

test4 = Var "a" `add` (num 3 `add` num 1)
test5 = UApp Neg (Var "b")

test6 = Quant Add ["a","i"] range body
   where range = Var "a" `lte` Var "i" `lte` Var "b"
         body = App (Var "b") (Var "i")

test71 = UApp Neg (num 3)
test72 = UApp Neg (Var "b")
test73 = (num 5) `add` (num 3)
test74 = UApp Neg (Var "b")
test78 = (Var "b")

test75 = ((Var "b") `add` (num 3))`add`((num 0) `add` (num 1))
test76 = ((num 5) `add` (num 3))`add`((num 0) `add` (num 1))
test8 = (Var "a" `add` Var "b") `eq` ((num 1 `mul` Var "a") `add` num 1)
test9 = Var "a" `eq` ((num 1 `mul` Var "a") `add` num 1)
test0 = Var "a" `add` (UApp Neg (Var "b"))
testlist = [("a",-2),("b",5)]

{-


--展開 : 遇到等式拆兩側、遇到計算往下拆
-}


-- tidy (BApp op (Lit(Num m)) (Lit(Num n))) = cal_unfold op (Lit (Num m)) (Lit (Num n))
-- --tidy (BApp op e1 e2) = BApp op (tidy e1) e2
-- tidy (BApp op e1 e2) = cal_unfold op (tidy e1) (tidy e2)







bind :: Maybe a -> (a -> Maybe b) -> Maybe b
m `bind` f = case m of
               Just n -> f n
               Nothing -> Nothing

--bind2 :: (a -> a -> Maybe b) -> Maybe a -> Maybe a -> Maybe b
bind2 :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
bind2 f m n = case (m , n) of
                (Just a,  Just b) -> f a b
                otherwise -> Nothing

iD :: a -> Maybe a
iD n = Just n


eval1 :: [(Name, Int)] -> Expr -> Maybe Int
eval1 _ (Lit (Num n)) = Just n
eval1 _ (Lit (Bol b)) = Nothing
eval1 [] (Var y) = Nothing
eval1 xs (Var y) = (lookup y xs) `bind` iD
      -- SCM: monad law, to be talked about later.
eval1 xs (UApp op e1) = (eval1 xs e1) `bind` (cal1 op)
eval1 xs (BApp op e1 e2) = -- bind2 (cal2 op) (eval1 xs e1) (eval1 xs e2)
  eval1 xs e1 `bind` \i ->
  eval1 xs e2 `bind` \j ->
  cal2 op i j

iDval ::  Int -> Maybe Val
iDval n = Just (VNum n)

bindval :: Maybe Val -> (Val -> Maybe Val) -> Maybe Val
m `bindval` f = case m of
               Just (VNum n) -> f (VNum n)
               Nothing -> Nothing


-- Exercise: extend eval
data Val = VNum Int | VBol Bool
          deriving (Eq, Show)

eval :: [(Name, Int)] -> Expr -> Maybe Val
eval _ (Lit (Num n)) = Just (VNum n)
eval _ (Lit (Bol b)) = Just (VBol b)
eval [] (Var y) = Nothing
eval xs (Var y) = (lookup y xs) `bind` iDval
eval xs (UApp op e1) = (eval xs e1) `bind` (cal0 op)
eval xs (BApp op e1 e2) = --bind2 (cal op) (eval xs e1) (eval xs e2)
      eval xs e1 `bind` \i ->
      eval xs e2 `bind` \j ->
      cal op i j

data POS = POS_Var (Name, Int)
         | POS_Num  Int
         | POS_BApp Op POS POS
              deriving (Eq, Show)
-- cal_POS :: Op -> POS -> POS -> POS



-- (4x + 2)+6
-- = 4x + (2+6)
-- = 4x + 8
-- BApp EQ  e1 e2
-- 1. e1, e2


{-
extend xs e1 = eval xs e1
extend xs Add e1 e2 = eval xs e1 e2
extend xs EQ e1 e2 = (extend xs e1, extend xs e2)
展開等式兩側做遞迴，直到式中沒有多層括號
當沒有括號做比對->完成整理

當都整理完了
帶入變數處理
比對（compare）兩側是否都有指定變數
移項
-}
-- data  ExprP = LitP Int
--            -- | VarP Name
--            | VarP Name Int
--            | AddP ExprP ExprP
--            | MulP ExprP ExprP
--            | NegP ExprP
--            deriving (Eq, Show)

{-
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
-}
type M a = Maybe a

-- bindM :: M a -> (a -> M b) -> M b


{-
  poly x (BApp Add (BApp Add (BApp Mul (Var "x")(num 3))(num 12))
                   (BApp Add (BApp Mul (num 5)(Var "x"))(num 7)))
= padd (poly x (BApp Add (BApp Mul (Var "x")(num 3))(num 12)))
       (poly x (BApp Add (BApp Mul (num 5)(Var "x"))(num 7))))

= padd (poly x (padd (poly x (BApp Mul (Var "x")(num 3))
                     (poly x (num 12)))

       (poly x (padd (poly x (BApp Mul (num 5)(Var "x"))
                     (poly x (num 7))))

= padd (poly x (padd (poly x (pmul (poly x (Var "x")
                                   (poly x (num 3))
                    (poly x (num 12)))

      (poly x (padd (poly x (pmul (poly x (num 5)
                                  (poly x (Var "x"))
                    (poly x (num 7))))

= padd (poly x (padd (poly x (pmul ([(Var "x")] [(num 3)])(poly x (num 12)))

      (poly x (padd (poly x (pmul (poly x (num 5)
                                  (poly x (Var "x"))
                    (poly x (num 7))))

= padd (poly x (padd (poly x (BApp Mul [(Var "x")] [(num 3)])[(num 12)]))


= poly x [3*x,12] [5*x,7]
= {poly x [3*x*5*x,12*5*x]} `padd` {poly x [3*x*7, 12*7]}
= [15, 60] []
= []

-}
--
-- f :: a -> a -> a -> [b]
-- f x y z= concat (map (\j -> concat (map (\i -> e3 i j)) (f y)) (f z))
--
-- e3 ::  a -> a -> a

test101 = BApp Add (BApp Mul (num 2) (Var "x")) (BApp Mul (BApp Mul (Var "x") (Var "x")) (num 2))
test102 = BApp Add  (Var "a")(num  2)
test103 = BApp Add (num 2) (num 2)
test104 = BApp Sub (num 2) (Var "a")
test105 = BApp Sub  (Var "a")(num  2)
test106 = BApp Sub (num 2) (num 2)
test107 = BApp Add (BApp Add (Var "a") (num 2)) (Var "a")
test108 = BApp Add (BApp Add (BApp Add (num 2) (num 2)) (num 3))(num 7)
test109 = BApp Add (BApp Add (Var "x") (num 2)) (num 3)
test110 = BApp Add (BApp Mul (Var "x") (num 2)) (num 3)

poly :: Name -> Expr -> [Expr]
poly _ (Lit n) = [Lit n]
-- poly _ (Var y) = [Var y]
poly x (Var y) | x == y = [Lit(Num 0), Lit(Num 1)]
               | otherwise = [(Var y)]
poly x (BApp Add e1 e2) = padd (poly x e1) (poly x e2)
--   --- padd [2+3y,3+y,4,0,2] [4, 3, z]
poly x (BApp Mul e1 e2) = pmul (poly x e1) (poly x e2)
--   --- pmul [2+3y,3+y,4,0,2] [4, 3, z]
poly x (UApp Neg e) = pneg (poly x e)

-- 0913-- after meeting
padd :: [Expr] -> [Expr] -> [Expr]
padd as [] = as
padd [] bs = bs
padd (a:as) (b:bs) = case (a,b) of
                    ((Lit (Num m)), (Lit (Num n))) -> [Lit (Num (m+n))] ++ padd as bs
                    otherwise -> [BApp Add a b] ++ padd as bs

pmul :: [Expr] -> [Expr] -> [Expr]
pmul [] bs = []
pmul (a:as) bs = padd (map(pmul1 a) bs) ([Lit (Num 0)]++(pmul as bs))

pmul1 :: Expr -> Expr -> Expr
pmul1 (Lit (Num m)) (Lit (Num n)) = (Lit (Num (m*n)))
pmul1 e1 e2 = BApp Mul e1 e2


pneg :: [Expr] -> [Expr]
pneg [e1] = [UApp Neg e1]

--                                             [Lit (Num 1)]
-- BApp Add (BApp Add (Var "b") (Lit (Num 3))) (BApp Add (Lit (Num 0)) (Lit (Num 1)))
{-0913
padd :: [Expr] -> [Expr] -> [Expr]
padd e1 [] = e1
padd [] e2 = e2
padd [Lit(Num m)] [Lit(Num n)] = [Lit(Num(m+n))]
padd [Lit(Num m)] [(Var y)]    = [BApp Add (Var y)(Lit(Num m))]
padd [(Var y)]    [Lit(Num m)] = [BApp Add (Var y)(Lit(Num m))]

pmul :: [Expr] -> [Expr] -> [Expr]
pmul [] [] = []
pmul xs [] = []
pmul [] ys = []
pmul [Lit(Num m)][Lit(Num n)] = [Lit(Num(m*n))]



padd :: [Expr] -> [Expr] -> [Expr]
padd e1 [] = e1
padd [] e2 = e2
-- padd e1 e2 = e1++e2
padd [Lit(Num m)] [Lit(Num n)] = [Lit(Num(m+n))]
padd [Lit(Num m)] [(Var y)]    = [BApp Add (Var y)(Lit(Num m))]
padd [(Var y)]    [Lit(Num m)] = [BApp Add (Var y)(Lit(Num m))]
padd [e1] [e2] = case (e1,e2) of
                (BApp Add (Var x)(Lit (Num n)) , Lit (Num m)) -> [BApp Add (Lit(Num (m+n))) (Var x)]
                (BApp Add (Var x)(Lit (Num n)) , (Var y)) -> if x == y then [(BApp Add (BApp Mul (Var x)(Lit(Num 2))) (Lit(Num n)))]
                                                                       else [BApp Add (BApp Add  (Var x) (Var y)) (Lit(Num n))]
                (BApp Add (Var x)(Lit (Num m)) ,(BApp Add (Var y)(Lit (Num n))) ) ->  if x == y then [BApp Add (Var x)(Lit(Num (m+n))) ]
                                                                                                else [BApp Add (BApp Add (Var x) (Var y)) (Lit(Num (m+n)))]

pmul :: [Expr] -> [Expr] -> [Expr]
pmul [] [] = []
pmul xs [] = []
pmul [] ys = []
pmul [Lit(Num m)][Lit(Num n)] = [Lit(Num(m*n))]
pmul [e1] [e2] = case (e1,e2) of
                (BApp Mul (Var x)(Lit (Num n)) , Lit (Num m)) -> [BApp Add (Lit(Num (m*n))) (Var x)]
                (BApp Mul (Var x)(Lit (Num n)) , (Var y)) -> if x == y then [(BApp Mul (BApp Mul (Var x)(Var x)) (Lit(Num n)))]
                                                                       else [BApp Mul (BApp Add  (Var x) (Var y)) (Lit(Num n))]
                (BApp Mul (Var x)(Lit (Num m)) ,(BApp Mul (Lit (Num n)) (Var y))) ->  if x == y then [BApp Mul (BApp Mul (Var x)(Var x)) (Lit(Num (m*n))) ]
                                                                                                else [BApp Mul (BApp Mul (Var x) (Var y)) (Lit(Num (m*n)))]

-}
-- pmul (x:xs) [y] = (pmul [x] [y]) ++ (pmul xs [y])
-- pmul xs (y:ys) = padd (pmul xs [y]) (pmul xs ys)

{-
padd :: [Expr] -> [Expr] -> [Expr]
padd e1 [] = e1
padd [] e2 = e2
padd [Lit(Num m)][Lit(Num n)] = [Lit(Num(m+n))]
padd [Lit(Num m)][(Var y)]    = [BApp Add (Lit(Num m)) (Var y)]
padd [(Var y)][Lit(Num m)]    = [BApp Add (Lit(Num m)) (Var y)]
-- padd ((Lit m):xs) ((Var y n):ys) = [BApp Add (Var y n) (Lit m)] ++ (padd xs ys)
-- padd ((Var y n):ys) ((Lit m):xs) = [BApp Add (Var y n) (Lit m)] ++ (padd xs ys)

-- padd [e1] [e2] = [BApp Add e1 e2]

pmul :: [Expr] -> [Expr] -> [Expr]
pmul [] [] = []
pmul xs [] = []
pmul [] ys = []
pmul [Lit(Num m)][Lit(Num n)] = [Lit(Num(m*n))]
pmul (x:xs) [y] = (pmul [x] [y]) ++ (pmul xs [y])
pmul xs (y:ys) = padd (pmul xs [y]) (pmul xs ys)


pneg :: [Expr] -> [Expr]
pneg [e1] = [UApp Neg e1]


poly :: Name -> ExprP -> [ExprP]
poly _ (LitP n) = [LitP n]
poly x (VarP y n) | x == y = [LitP n]
                  | otherwise = [VarP y n]
poly x (AddP e1 e2) = padd (poly x e1) (poly x e2)
--   --- padd [2+3y,3+y,4,0,2] [4, 3, z]
poly x (MulP e1 e2) = pmul (poly x e1) (poly x e2)
--   --- pmul [2+3y,3+y,4,0,2] [4, 3, z]
-- poly x (NegP e) = pneg (poly x e)


padd :: [ExprP] -> [ExprP] -> [ExprP]
padd [] [] = []
padd e1 [] = e1
padd [] e2 = e2
padd ((LitP m):xs) ((LitP n):ys) = [LitP (m+n)] ++ (padd xs ys)

padd ((VarP x m):xs) ((VarP y n):ys) | x == y = [VarP x (m+n)] ++ (padd xs ys)
                                     | otherwise = [AddP (VarP x m) (VarP y n)]++ (padd xs ys)

padd ((LitP m):xs) ((VarP y n):ys) = [AddP (VarP y n) (LitP m)] ++ (padd xs ys)
padd ((VarP y n):ys) ((LitP m):xs) = [AddP (VarP y n) (LitP m)] ++ (padd xs ys)

pmul :: [ExprP] -> [ExprP] -> [ExprP]
pmul [] [] = []
pmul e1 [] = []
pmul [] e2 = []
pmul [(LitP m)]   [(LitP n)]   = [LitP (m*n)]
pmul [(LitP m)]   [(VarP x n)] = [VarP x (m*n)]
pmul [(VarP x n)] [(LitP m)]   = [VarP x (m*n)]
pmul [(VarP x m)] [(VarP y n)] = [MulP (VarP x m) (VarP y m)]
pmul (x:xs) [y] = (pmul [x] [y]) ++ (pmul xs [y])
pmul xs (y:ys) = padd (pmul xs [y]) (pmul xs ys)
--pmul xs ((LitP n):ys) = (map (pmul [LitP n]) xs) padd (pmul xs ys)
-- pmul xs ys = []

-- -- pmulcd
-- --
-- pneg :: [ExprP] -> [ExprP]
-- pneg xs = []
-- pneg

-}

{-
poly :: Name -> ExprP -> M [ExprP]
poly _ (Lit n) = return [Lit n]
poly x (Var y) | x == y = .....
               | otherwise = ...
poly x (Add e1 e2) = padd (poly x e1) (poly x e2)
  --- padd [2+3y,3+y,4,0,2] [4, 3, z]
poly x (Mul e1 e2) = pmul (poly x e1) (poly x e2)
  --- pmul [2+3y,3+y,4,0,2] [4, 3, z]
poly x (Neg e) = pneg (poly x e)
-}

-- 0809
-- algorithm-to-solve-linear-equation-in-one-variable
-- shiftr :: Expr -> Expr
-- shiftr (BApp op e1 e2) = case op of
--                             Eq -> case e1 of


--
-- shiftr (Var "a" `eq` ((num 1 `mul` Var "a") `add` num 1)) = Var "a" `eq` num (-1)
--
-- shiftl ::

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

0804
* Exercise: refactor the code and make the
  "application of binary operators" a separated function.

* Exercise: rewrite eval1 and eval using bind.

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


eval1 :: [(Name, Int)] -> Expr -> Maybe Int
eval1 _ (Lit (Num n)) = Just n
eval1 _ (Lit (Bol b)) = Nothing
eval1 [] (Var y) = Nothing
--(Just xs) `bind` (lookup y)
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
0825
testlist1 = [(Var "a",Just 1),(Var "cons",Just 2)]
tidy :: [(Expr, Maybe Int)] -> [(Expr, Maybe Int)]
tidy [] = []
tidy [(Var a, Just n)] = [(Var a, Just n)]
tidy (x:xs) = [x] ++ (filter (\(a,_) -> a == fst x) xs) ++ tidy (filter (\(a,_) -> a /= fst x) xs)

tidy_sum :: [(Expr, Maybe Int)] -> [(Expr, Maybe Int)]
tidy_sum [(Var a,Just m),(Var a,Just n)] = :

second :: [(Expr, Maybe Int)] -> Maybe Int
second [(Var a, Just n)] = Just n

--sum_maybe :: Maybe Int -> Maybe Int

-- tidy_sum :: [(Expr, Maybe Int)] -> [(Expr, Maybe Int)]
-- tidy_sum [(Var m, Just n)]x++xs = [(Var m, sum_maybe(second(x:xs)))]
--
-- tidy_sum :: [(Expr, Maybe Int)] -> [(Expr, Maybe Int)]
-- tidy_sum (x++xs) = [(fst x, sum_maybe(second(x:xs)))]


convert ::  Expr -> [(Expr, Maybe Int)]
convert (Lit(Num n)) = [(Var "cons", Just n)]
convert (Var m) = [(Var m, Just 1)]
convert (UApp Neg (Lit(Num n))) = [(Var "cons", Just (-n))]
convert (UApp Neg (Var m)) = [(Var m, Just (-1))]
convert (BApp op (Lit(Num m)) (Lit(Num n))) = case op of
                                              Add -> [(Var "cons", Just (m+n))]
                                              Sub -> [(Var "cons", Just (m-n))]
                                              Mul -> [(Var "cons", Just (m*n))]
                                              Div -> case n of
                                                        0 -> [(Var "cons", Nothing)]
                                                        otherwise -> [(Var "cons", Just (div m n))]

-- convert (BApp Add (Lit(Num m)) (Lit(Num n))) = [(Var "cons", Just (m+n))]
-- convert (BApp Sub (Lit(Num m)) (Lit(Num n))) = [(Var "cons", Just (m-n))]
-- convert (BApp Mul (Lit(Num m)) (Lit(Num n))) = [(Var "cons", Just (m*n))]
-- convert (BApp Div (Lit(Num m)) (Lit(Num n))) = case n of
--                                                 0 -> [(Var "cons", Nothing)]
--                                                 otherwise -> [(Var "cons", Just (div m n))]
-- convert [] (BApp op e1 e2) = case (e1, e2) of
--                               ((Lit(Num m),(Lit(Num n)) -> [("cons", Just (cal2 op m n)]

convert (BApp Add e1 e2) = convert e1 ++ convert e2
shift Var "n"`lte` (Var "a" `add` num 1 ) =

move ::
move xs op e1 e2 = case op of
                Add -> move op (open xs e1) (open xs e2)
                EQ ->  (extend xs e1 , extend xs e2)

-}
