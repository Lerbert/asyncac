module KIV.Types (module KIV.Types, module Common.Types) where

import Common.Types

newtype ProjectName = ProjectName String
    deriving (Eq, Ord, Show)

newtype SpecName = SpecName String
    deriving (Eq, Ord, Show)

newtype KIVName = KIVName String
    deriving (Eq, Ord, Show)

newtype KIVType = KIVType String
    deriving (Eq, Ord, Show)

data SimplifierRule = Local | Global
    | LocalForward | GlobalForward -- ...
    deriving (Eq, Ord, Show)

data Constant = Constant KIVName KIVType
    deriving (Eq, Ord, Show)

data Function = Function KIVName [KIVType] KIVType
    deriving (Eq, Ord, Show)

data Predicate = Predicate KIVName [KIVType]
    deriving (Eq, Ord, Show)

data Axiom = Axiom String Formula [SimplifierRule]
    deriving (Eq, Ord, Show)

data Lemma = Lemma String Formula [SimplifierRule]
    deriving (Eq, Ord, Show)

data SpecItems = SpecItems [Constant] [Function] [Predicate] [Axiom]
    deriving (Eq, Ord, Show)

data DataConstructor = DataConstructor KIVName [(KIVName, KIVType)] -- constructor [(accessor, paramtype)]
    deriving (Eq, Ord, Show)

data DataDefinition = DataDefinition KIVType [DataConstructor]
    deriving (Eq, Ord, Show)

-- only a subset of KIV's heuristics, add more as needed
data Heuristic = Simplifier | Elimination | NonsplittingCut | UsePatterns | QuantifierClosing | WeakCut | StructuralInduction | Cut | ITESplit | Quantifier | PLCaseDistinction
    deriving (Eq, Ord, Show)

plHeurSet :: [Heuristic]
plHeurSet = [Simplifier, Elimination, UsePatterns, QuantifierClosing]

plCaseSplitHeurSet :: [Heuristic]
plCaseSplitHeurSet = [Simplifier, Elimination, NonsplittingCut, UsePatterns, QuantifierClosing, WeakCut, ITESplit]

plStructInductionHeurSet :: [Heuristic]
plStructInductionHeurSet = [Simplifier, Elimination, NonsplittingCut, UsePatterns, QuantifierClosing, StructuralInduction, Cut, ITESplit, PLCaseDistinction]

plCaseSplitQuantifierHeurSet :: [Heuristic]
plCaseSplitQuantifierHeurSet = [Simplifier, Elimination, NonsplittingCut, UsePatterns, QuantifierClosing, Cut, ITESplit, Quantifier, PLCaseDistinction]

data LibraryImport = LibraryImport ProjectName [SpecName]
    deriving (Eq, Ord, Show)

data KIVFile = Enrich SpecName [SpecName] SpecItems -- spec enriched items
    -- | Basic SpecName [SpecName] SpecItems -- spec used items
    -- ...
    | Data SpecName [SpecName] [DataDefinition]
    | Instantiate SpecName SpecName SpecName SpecName -- spec generic param actual
    | Rename SpecName SpecName [(KIVType, KIVType)] [(KIVName, KIVName)] -- spec renamed type_renames symbol_renames
    | Sequents SpecName [Lemma]
    | Heuristics SpecName [Heuristic]
    | Libraries [LibraryImport]
    deriving (Eq, Ord, Show)
