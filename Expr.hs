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
binOp op e1 e2 = App (App (Op op) e1) e2

add = binOp Add
mul = binOp Mul
lte = binOp LTE

test1 = ((num 3 `add` num 4) `add` num 5) `mul` (num 6)
test2 = Quant (Op Add) ["i"] range body
   where range = Var "m" `lte` Var "i" `lte` Var "n"
         body = App (Var "f") (Var "i")
