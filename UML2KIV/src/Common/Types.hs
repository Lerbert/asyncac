module Common.Types where

data Formula = OpaqueFormula String
    | TrueF
    | FalseF
    | Equality Expression Expression
    | Not Formula
    | And [Formula]
    | Or [Formula]
    | Imp Formula Formula
    | Iff Formula Formula
    deriving (Eq, Ord, Show)

data Expression = OpaqueExpression String
    | VariableExpr Variable
    deriving (Eq, Ord, Show)

newtype Variable = Variable String
    deriving (Eq, Ord, Show)

data AssumptionCommitmentFormula = AssumptionCommitmentFormula {
    assumption :: Formula,
    commitment :: Formula,
    precondition :: Formula
}
    deriving (Eq, Ord, Show)
