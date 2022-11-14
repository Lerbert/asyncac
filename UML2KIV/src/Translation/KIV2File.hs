module Translation.KIV2File where

import qualified Data.Text as T
import System.FilePath ((</>), (<.>))

import qualified KIV.Types as KIV
import Translation.Common2Text

specPath (KIV.SpecName name) = "specs" </> name </> "specification" <.> "utf8"
sequentsPath (KIV.SpecName name) = "specs" </> name </> "sequents" <.> "utf8"
heuristicsPath (KIV.SpecName name) = "specs" </> name </> "heuristics"
librariesPath = "libraries"

translateKIVFile :: KIV.KIVFile -> (FilePath, T.Text)
translateKIVFile (KIV.Enrich spec enriched items) = (specPath spec, translateEnrich enriched items)
translateKIVFile (KIV.Data spec used dataDefs) = (specPath spec, translateData used dataDefs)
translateKIVFile (KIV.Instantiate spec generic param actual) = (specPath spec, translateInstantiate generic param actual)
translateKIVFile (KIV.Rename spec renamed typeRenames symbolRenames) = (specPath spec, translateRename renamed typeRenames symbolRenames)
translateKIVFile (KIV.Sequents spec lemmas) = (sequentsPath spec, translateSequents lemmas)
translateKIVFile (KIV.Heuristics spec heuristics) = (heuristicsPath spec, translateHeuristics heuristics)
translateKIVFile (KIV.Libraries libImports) = (librariesPath, translateLibraries libImports)

translateEnrich enriched items =
    T.pack "enrich " <> T.intercalate (T.pack ", ") (translateSpecName <$> enriched) <> T.pack " with\n\n"
    <> translateSpecItems items <> T.pack "\n"
    <> T.pack "end enrich"

translateData used dataDefs =
    T.pack "data specification\n\tusing " <> T.intercalate (T.pack ", ") (translateSpecName <$> used) <> T.pack "\n\n"
    <> terminateWith (T.pack ";\n") (translateDataDefinition <$> dataDefs) <> T.pack "\n"
    <> genSpecPart "variables" (T.pack "n : nat" : variables) <> T.pack "\n" -- TODO: make it so that n is not required
    <> T.pack "end data specification"
    where variables = mkVariable <$> dataDefs
          mkVariable (KIV.DataDefinition ktype _) = T.pack "var" <> translateKIVType ktype <> T.pack " : " <> translateKIVType ktype

translateInstantiate generic param actual =
    T.pack "instantiate parameter " <> translateSpecName param <> T.pack " < " <> translateSpecName generic <> T.pack " with " <> translateSpecName actual <> T.pack " by mapping\n"
    <> T.pack "\n"
    <> T.pack "end instantiate"

translateRename renamed typeRenames symbolRenames =
    T.pack "rename " <> translateSpecName renamed <> T.pack " by morphism\n\n"
    <> terminateWith (T.pack ";\n") (translateMapping <$> (typeRenamesS <> symbolRenamesS)) <> T.pack "\n"
    <> T.pack "end rename"
    where typeRenamesS = map (both translateKIVType) typeRenames
          symbolRenamesS = map (both translateKIVName) symbolRenames
          both f (a, b) = (f a, f b)
          translateMapping (src, dst) = src <> T.pack " → " <> dst

translateSequents = mappend (T.pack "lemmas") . genSpecPart "" . fmap translateLemma

translateHeuristics heuristics = terminateWith (T.pack "\n") (translateHeuristic <$> heuristics)

translateHeuristic KIV.Simplifier = T.pack "simplifier"
translateHeuristic KIV.Elimination = T.pack "elimination"
translateHeuristic KIV.NonsplittingCut = T.pack "nonsplitting cut"
translateHeuristic KIV.UsePatterns = T.pack "use patterns"
translateHeuristic KIV.QuantifierClosing = T.pack "Quantifier closing"
translateHeuristic KIV.WeakCut = T.pack "weak cut"
translateHeuristic KIV.StructuralInduction = T.pack "structural induction"
translateHeuristic KIV.Cut = T.pack "cut"
translateHeuristic KIV.ITESplit = T.pack "if-then-else split"
translateHeuristic KIV.Quantifier = T.pack "Quantifier"
translateHeuristic KIV.PLCaseDistinction = T.pack "pl case distinction"

translateLibraries imports = terminateWith (T.pack "\n") (translateLibraryImport <$> imports)

translateLibraryImport (KIV.LibraryImport libName specs) = T.intercalate (T.pack "\n") $ map ((<>(T.pack " spec " <> translateProjectName libName)) . translateSpecName) specs

translateProjectName (KIV.ProjectName name) = T.pack name

translateSpecName (KIV.SpecName name) = T.pack name

translateKIVName (KIV.KIVName name) = T.pack name

translateKIVType (KIV.KIVType ktype) = T.pack ktype

translateSimplifierRule KIV.Local = T.pack "ls"
translateSimplifierRule KIV.Global = T.pack "s"
translateSimplifierRule KIV.LocalForward = T.pack "lf"
translateSimplifierRule KIV.GlobalForward = T.pack "f"

translateSimplifierRules rules = if null rules then T.empty else T.pack "; used for: " <> T.intercalate (T.pack ", ") (translateSimplifierRule <$> rules)

translateConstant (KIV.Constant name ktype) = translateKIVName name <> T.pack " : " <> translateKIVType ktype

translateFunction (KIV.Function name paramTypes retType) = translateKIVName name <> T.pack " : " <> T.intercalate (T.pack " × ") (translateKIVType <$> paramTypes) <> T.pack " → " <> translateKIVType retType

translatePredicate (KIV.Predicate name paramTypes) = translateKIVName name <> T.pack " : " <> T.intercalate (T.pack " × ") (translateKIVType <$> paramTypes)

translateAxiom (KIV.Axiom name f simplifier) = T.pack name <> T.pack ": " <> translateFormulaIndent f <> translateSimplifierRules simplifier

translateLemma (KIV.Lemma name f simplifier) = T.pack name <> T.pack ": " <> translateFormulaIndent f <> translateSimplifierRules simplifier

translateSpecItems (KIV.SpecItems constants functions predicates axioms) =
    genSpecPart "constants" (translateConstant <$> constants) <> T.pack "\n"
    <> genSpecPart "functions" (translateFunction <$> functions) <> T.pack "\n"
    <> genSpecPart "predicates" (translatePredicate <$> predicates) <> T.pack "\n"
    <> genSpecPart "axioms" (translateAxiom <$> axioms) <> T.pack "\n"

genSpecPart keyword items =
    if null items
        then T.pack ";; " <> T.pack keyword <> T.pack "\n"
        else T.pack keyword <> T.pack "\n" <> (T.unlines . fmap (mappend (T.pack "\t")) . T.lines) (terminateWith (T.pack ";\n") items)

translateDataConstructor (KIV.DataConstructor name params) = translateKIVName name <> genParamList (T.pack "; ") (translateConsParam <$> params)
    where translateConsParam (accessor, ktype) = translateKIVName accessor <> T.pack " : " <> translateKIVType ktype

genParamList sep params = if null params then T.empty else T.singleton '(' <> T.intercalate sep params <> T.singleton ')'

translateDataDefinition (KIV.DataDefinition ktype constructors) = translateKIVType ktype <> T.pack " = " <> T.intercalate (T.pack " | ") (translateDataConstructor <$> constructors)

terminateWith sep text = T.intercalate sep text <> if null text then T.empty else sep

translateFormulaIndent = translateFormulaIndenth 0

translateFormulaIndenth depth (KIV.Imp lhs rhs) = indentFormula depth lhs " → " rhs
translateFormulaIndenth depth (KIV.Iff lhs rhs) = indentFormula depth lhs " ↔ " rhs
-- remaining cases analog to translateFormula
translateFormulaIndenth depth (KIV.OpaqueFormula f) = T.pack f
translateFormulaIndenth depth KIV.TrueF = T.pack "true"
translateFormulaIndenth depth KIV.FalseF = T.pack "false"
translateFormulaIndenth depth (KIV.Equality lhs rhs) = translateExpression lhs <> T.pack " = " <> translateExpression rhs
translateFormulaIndenth depth (KIV.Not f) = T.pack "¬ " <> paren (translateFormulaIndenth depth f)
translateFormulaIndenth depth (KIV.And fs) =  intercalateMapNonEmpty (paren . translateFormulaIndenth depth) " ∧ " KIV.TrueF fs
translateFormulaIndenth depth (KIV.Or fs) = intercalateMapNonEmpty (paren . translateFormulaIndenth depth) " ∨ " KIV.FalseF fs

indentFormula depth lhs op rhs = paren (translateFormulaIndenth depth lhs) <> T.pack op <> paren (T.singleton '\n' <> tabs (depth + 1) <> translateFormulaIndenth (depth + 1) rhs <> T.singleton '\n' <> tabs depth)
    where tabs d = T.pack (replicate d '\t')