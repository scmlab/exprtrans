{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Pretty where
import Prelude hiding (EQ, GT, LT)
import Expr

 -- SPrint for "simple print"

class SPrint a where
  sprint :: a -> String
  sprints :: a -> String -> String
  sprintsPrec :: Int -> a -> String -> String

  sprint x = sprintsPrec 0 x []
  sprints x = sprintsPrec 0 x
  sprintsPrec _ x = sprints x

precApply :: Int
precApply = 12

instance SPrint Expr where
 sprintsPrec _ (Var x) = (x++)
 sprintsPrec _ (Lit n) = sprints n
 sprintsPrec _ (Op op) = sprints op
 sprintsPrec p (App (App (Op op) e1) e2) =
   case precedence op of
     Infix  n -> showParen (p > n) $
                  sprintsPrec (n+1) e1 . (' ':) . sprints op . (' ':) .
                    sprintsPrec (n+1) e2
     InfixL n -> showParen (p > n) $
                  sprintsPrec n e1 . (' ':) . sprints op . (' ':) .
                    sprintsPrec (n+1) e2
     InfixR n -> showParen (p > n) $
                  sprintsPrec (n+1) e1 . (' ':) . sprints op . (' ':) .
                    sprintsPrec n e2
     Prefix n -> showParen (p > precApply) $
                   (showParen True $ sprints op . (' ':) .
                     sprintsPrec 11 e1) . (' ':) . sprintsPrec (precApply + 1) e2
     Postfix n -> showParen (p > precApply) $
                   (showParen True $ sprintsPrec (precApply + 1) e1. (' ':) .
                      sprints op) . (' ':) . sprintsPrec (precApply + 1) e2
 sprintsPrec p (App (Op op) e) =
         showParen (p > precApply) $
        ('(':) . sprints op . (") " ++) . sprintsPrec (precApply + 1) e
 sprintsPrec p (App e1 e2) =
    showParen (p > precApply) $
      sprintsPrec (precApply + 1) e1 . (' ':) .
      sprintsPrec (precApply + 1) e2
 sprintsPrec _ (Quant eop xs range body) =
   ('⟨':) . sprintQOp eop . (' ':) . sprintsSep (' ':) xs .
    (" : " ++) . sprints range .
    (" : " ++) . sprints body . ('⟩':)
  where sprintQOp (Op op) = sprints op
        sprintQOp e = sprintsPrec precApply e

{-

The general rule is

infix n: use showParen (p > n), showsPrec (n+1) on the left, and showsPrec (n+1) on the right
infixl n: use showParen (p > n), showsPrec n on the left, and showsPrec (n+1) on the right
infixr n: use showParen (p > n), showsPrec (n+1) on the left, and showsPrec n on the right
non-infix: use showParen (p > 10) and showsPrec 11 on the arguments

-}

sprintsSep _ [] = id
sprintsSep _ [x] = sprintsPrec precApply x
sprintsSep sep (x:xs) =
  sprintsPrec precApply x . sep . sprintsSep sep xs

instance SPrint Name where
  sprints x = (x++)

instance SPrint Lit where
  sprintsPrec _ (Num n) = (show n ++)
  sprintsPrec _ (Bol b) = (show b ++)

instance SPrint Op where
  sprints EQ = ('=':) -- relations
  sprints NEQ = ('≠':)
  sprints LTE = ('≤':)
  sprints GTE = ('≥':)
  sprints LT = ('<':)
  sprints GT = ('>':)
  sprints Implies = ('⇒':)
  sprints Conj = ('∧':)
  sprints Disj = ('∨':)
  sprints Neg = ('¬':)
  sprints Add = ('+':)
  sprints Sub = ('-':)
  sprints Mul = ('*':)
  sprints Div = ('/':)
  sprints Mod = ("mod" ++)
  sprints Max = ('↑':)
  sprints Min = ('↓':)
  sprints Exp = ('^':)
