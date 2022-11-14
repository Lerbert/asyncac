module Translation.Common2Text where

import qualified Data.Text as T

import Common.Types

translateFormula :: Formula -> T.Text
translateFormula (OpaqueFormula f) = T.pack f
translateFormula TrueF = T.pack "true"
translateFormula FalseF = T.pack "false"
translateFormula (Equality lhs rhs) = translateExpression lhs <> T.pack " = " <> translateExpression rhs
translateFormula (Not f) = T.pack "¬ " <> paren (translateFormula f)
translateFormula (And fs) =  intercalateMapNonEmpty (paren . translateFormula) " ∧ " TrueF fs
translateFormula (Or fs) = intercalateMapNonEmpty (paren . translateFormula) " ∨ " FalseF fs
translateFormula (Imp lhs rhs) = paren (translateFormula lhs) <> T.pack " → " <> paren (translateFormula rhs)
translateFormula (Iff lhs rhs) = paren (translateFormula lhs) <> T.pack " ↔ " <> paren (translateFormula rhs)

paren t = T.singleton '(' <> t <> T.singleton ')'
intercalateMapNonEmpty f sep def l = if null l then f def else T.intercalate (T.pack sep) $ f <$> l

translateExpression :: Expression -> T.Text
translateExpression (OpaqueExpression e) = T.pack e
translateExpression (VariableExpr var) = translateVariable var

translateVariable :: Variable -> T.Text
translateVariable (Variable v) = T.pack v
