module Translation.AsyncAC2KIV where

import Control.Monad (join)
import Data.List ((\\), intercalate)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Set as Set

import qualified AsyncAC.Types as AC
import qualified KIV.Types as KIV

channelType = KIV.KIVType "Channel"
eventType = KIV.KIVType "Event"
ctrlType = KIV.KIVType "Ctrl"
namedCtrlType name = KIV.KIVType $ tsNameToString name ++ "Ctrl"
dataType = KIV.KIVType "Data"
namedDataType name = KIV.KIVType $ tsNameToString name ++ "Data"
dataCons = KIV.KIVName "Dt"
namedDataCons name = KIV.KIVName $ tsNameToString name ++ "Dt"
historyType = KIV.KIVType "History"
confType = KIV.KIVType "Conf"
namedConfType name = KIV.KIVType $ tsNameToString name ++ "Conf"
confCons = KIV.KIVName "conf"
namedConfCons name = KIV.KIVName $ tsNameToString name ++ "cConf"

--asmpt
asmpt = "asmpt"
asmptFormula hist = KIV.OpaqueFormula $ asmpt ++ "(" ++ hist ++ ")"
asmptPred = KIV.Predicate (KIV.KIVName asmpt) [historyType]
asmptAxiom = genDefAxiomF asmpt (asmptFormula "h") . maybe KIV.TrueF AC.assumption

-- cmt
cmt = "cmt"
cmtFormula hist = KIV.OpaqueFormula $ cmt ++ "(" ++ hist ++ ")"
cmtPred = KIV.Predicate (KIV.KIVName cmt) [historyType]
cmtAxiom = genDefAxiomF cmt (cmtFormula "h") . maybe KIV.TrueF AC.commitment

-- assertions
assertionPrefix = "q_"
assertionName cs = assertionPrefix ++ controlStateToString cs
assertionFormula cs hist conf = KIV.OpaqueFormula $ assertionName cs ++ "(" ++ hist ++ ", " ++ conf ++ ")"
-- q_init
initAssertion = assertionPrefix ++ "init"
initAssertionFormula conf = KIV.OpaqueFormula $ initAssertion ++ "(" ++ conf ++ ")"
initAssertionPred name = KIV.Predicate (KIV.KIVName initAssertion) [namedConfType name]
initAssertionAxiom = genDefAxiomF initAssertion (initAssertionFormula "g") . maybe KIV.TrueF AC.precondition

-- completeness
completeFormula hist inputs = KIV.OpaqueFormula $ "complete(" ++ hist ++ ", " ++ inputSet ++ ")"
    where inputSet = listToKIVSet $ mapSetToList channelToString inputs

-- =============================
-- | TRANSITION SYSTEM --> KIV |
-- =============================

translateTransitionSystem :: KIV.SpecName -> AC.TransitionSystem -> [KIV.KIVFile]
translateTransitionSystem historySpec (AC.TransitionSystem name ctrlStates variables inputs outputs transitions initCtrl initPred acp assertions) = [systemConf, instantiateConf, renameConf, system, sequents, heuristics]
    where systemConf = genSystemConf name ctrlStates variables
          instantiateConf = genInstantiateConf name
          renameConf = genRenameConf name
          system = genSystem historySpec name ctrlStates inputs outputs acp assertions
          sequents = genSequents name ctrlStates variables inputs transitions initCtrl initPred
          heuristics = KIV.Heuristics (KIV.SpecName $ tsNameToString name) KIV.plCaseSplitHeurSet

genSystemConf name ctrlStates variables = KIV.Data (KIV.SpecName $ tsNameToString name ++ "-ctrl-data") [KIV.SpecName "nat"] [ctrl, dt]  -- TODO: all vars are nat for now
    where ctrl = genCtrlType ctrlStates
          dt = genDataType variables

genCtrlType ctrlStates = KIV.DataDefinition ctrlType constructors
    where constructors = mapSetToList mkConstructor ctrlStates
          mkConstructor cs = KIV.DataConstructor (KIV.KIVName $ controlStateToString cs) []

genDataType variables = KIV.DataDefinition dataType [KIV.DataConstructor dataCons params]
    where params = mapSetToList mkParam variables
          mkParam var = (varToAccessor var, KIV.KIVType "nat") -- TODO: all vars are nat for now

varToAccessor = KIV.KIVName . (++"Acc") . varToString

genInstantiateConf name = KIV.Instantiate (KIV.SpecName $ tsNameToString name ++ "-conf-inst") (KIV.SpecName "conf") (KIV.SpecName "ctrl-data") (KIV.SpecName $ tsNameToString name ++ "-ctrl-data")

genRenameConf name = KIV.Rename (KIV.SpecName $ tsNameToString name ++ "-conf") (KIV.SpecName $ tsNameToString name ++ "-conf-inst") typeRenames symbolRenames
    where typeRenames = [(confType, namedConfType name), (ctrlType, namedCtrlType name), (dataType, namedDataType name)]
          symbolRenames = [(confCons, namedConfCons name), (dataCons, namedDataCons name)]

genSystem historySpec name ctrlStates inputs outputs acp assertions = KIV.Enrich (KIV.SpecName $ tsNameToString name) [KIV.SpecName $ tsNameToString name ++ "-conf", historySpec] items
    where items = KIV.SpecItems constants [] (asmptPred : cmtPred : initAssertionPred name : assertionPredicates) (disjAxioms <> (asmptAxiom acp : cmtAxiom acp : initAssertionAxiom acp : assertionAxioms))
          constants = mapSetToList mkConstant (inputs <> outputs)
          mkConstant chan = KIV.Constant (KIV.KIVName $ channelToString chan) channelType
          assertionPredicates = mapSetToList mkAssertion ctrlStates
          mkAssertion c = KIV.Predicate (KIV.KIVName $ assertionName c) [historyType, namedConfType name]
          disjAxioms = genDisjointAxioms $ mapSetToList id (inputs <> outputs)
          assertionAxioms = mapSetToList mkAssertionAxiom ctrlStates
          mkAssertionAxiom c = genDefAxiomF (assertionName c) (assertionFormula c "h" "g") (fromMaybe KIV.TrueF $ lookupAssertion c)
          lookupAssertion c = join $ lookup c assertions

genDisjointAxioms chans = mkAxiom <$> channelPairs
    where mkAxiom (c1, c2) = let (name1, name2) = (channelToString c1, channelToString c2) in
            KIV.Axiom (name1 ++ "-" ++ name2 ++ "-disj") (KIV.Not $ KIV.Equality (KIV.OpaqueExpression name1) (KIV.OpaqueExpression name2)) [KIV.Global, KIV.Local]
          channelPairs = [(c1, c2) | c1 <- chans, c2 <- chans, c1 < c2]

genSequents name ctrlStates variables inputs transitions initCtrl initPred = KIV.Sequents (KIV.SpecName $ tsNameToString name) (cmtInit : assertionInit : transLemmas <> inputLemmas)
    where cmtInit = KIV.Lemma (cmt ++ "-init") (cmtFormula "[]") []
          assertionInit = genAssertionInitLemma variables initCtrl initPred
          transLemmas = zipWith (genTransLemma variables inputs) [1..] $ mapSetToList id transitions
          inputLemmas = mapSetToList (genInputLemma inputs) ctrlStates

genAssertionInitLemma variables initState initPred = KIV.Lemma name (KIV.Imp premise consequence) []
    where name = initAssertion ++ "-" ++ controlStateToString initState
          premise = KIV.And [initAssertionFormula "g", ctrlState, variablesFromConf variables "g", fromMaybe KIV.TrueF initPred]
          ctrlState = KIV.Equality (confCtrlExpr "g") $ KIV.OpaqueExpression $ controlStateToString initState
          consequence = assertionFormula initState "[]" "g"

genTransLemma variables inputs i t@(AC.Trans src _ _ _ assigns dst) = if AC.isInternal t then genInternalTransLemma nextConf inputs lemmaName t else genCommTransLemma nextConf inputs lemmaName t
    where lemmaName = show i ++ "-" ++ controlStateToString src ++ "-" ++ controlStateToString dst
          varList = mapSetToList id variables
          assignVars = map (\(AC.Assignment var _) -> var) assigns
          keepVars = varList \\ assignVars
          nextConf = KIV.And $ nextCtrlState : variablesFromConf variables "g" : (mkAssignFormula <$> assigns) <> (mkKeepFormula <$> keepVars)
          nextCtrlState = KIV.Equality (confCtrlExpr "g1") $ KIV.OpaqueExpression $ controlStateToString dst
          mkAssignFormula (AC.Assignment var expr) = KIV.Equality (confAccessorExpr (varToAccessor var) "g1") expr
          mkKeepFormula var = KIV.Equality (confAccessorExpr (varToAccessor var) "g1") (KIV.VariableExpr  var)

confAccessorExpr (KIV.KIVName accessor) conf = KIV.OpaqueExpression $ accessor ++ "(data(" ++ conf ++ "))"
confCtrlExpr conf = KIV.OpaqueExpression $ "ctrl(" ++ conf ++ ")"

variablesFromConf variables conf = KIV.And $ mapSetToList mkEquality variables
    where mkEquality var = KIV.Equality (KIV.VariableExpr var) (confAccessorExpr (varToAccessor var) conf)

genInternalTransLemma nextConf inputs name (AC.Trans src _ guard _ _ dst) = KIV.Lemma name (KIV.Imp prepareVars acInductive) []
    where prepareVars = KIV.And [nextConf, fromMaybe KIV.TrueF guard]
          acInductive = KIV.Imp premise consequence
          premise = KIV.And [assertionFormula src "h" "g", asmptFormula "h", completeFormula "h" inputs]
          consequence = assertionFormula dst "h" "g1"

genCommTransLemma nextConf inputs name (AC.Trans src receives guard sends _ dst) = KIV.Lemma name (KIV.Imp prepareVars acInductive) []
    where prepareVars = KIV.And [KIV.Equality (KIV.OpaqueExpression "h1") nextHistory, nextConf, fromMaybe KIV.TrueF guard]
          nextHistory = KIV.OpaqueExpression $ "(" ++ mkHistoryItem sends receives ++ ") + h"
          acInductive = KIV.Imp premise consequence
          premise = KIV.And [assertionFormula src "h" "g", asmptFormula "h", completeFormula "h1" inputs]
          consequence = KIV.And [cmtFormula "h1", KIV.Imp (asmptFormula "h1") (assertionFormula dst "h1" "g1")]

mkHistoryItem sends receives = listToKIVSet messages
    where messages = mapSetToList sendDeclToMsg sends <> mapSetToList receiveDeclToMsg receives

genSendOp chan = "send(" ++ channelToString chan ++ ")"
genReceiveOp chan = "receive(" ++ channelToString chan ++ ")"
genMsg op event = "msg(" ++ op ++ ", " ++ event ++ ")"

sendDeclToMsg (AC.SendDecl chan evName params) = genMsg (genSendOp chan) (evNameToString evName ++ paramsS)
    where paramsS = paramList exprToString ", " "" params

receiveDeclToMsg (AC.ReceiveDecl chan evName params) = genMsg (genReceiveOp chan) (evNameToString evName ++ paramsS)
    where paramsS = paramList varToString ", " "" params

paramList f sep def l = if null l then def else '(' : intercalate sep (f <$> l) ++ ")"

genInputLemma inputs cs = KIV.Lemma (controlStateToString cs ++ "-input") (KIV.Imp prepareVars acInductive) []
    where prepareVars = KIV.And [KIV.Equality (KIV.OpaqueExpression "h1") (KIV.OpaqueExpression "i + h"), KIV.OpaqueFormula $ "i ⊆ " ++ inputMsgSet, KIV.Not $ KIV.Equality (KIV.OpaqueExpression "i") (KIV.OpaqueExpression "∅")]
          inputMsgSet = listToKIVSet $ zipWith chanToSend [0..] $ mapSetToList id inputs
          chanToSend i chan = genMsg (genSendOp chan) ("e" ++ show i)
          acInductive = KIV.Imp premise consequence
          premise = KIV.And [assertionFormula cs "h" "g", asmptFormula "h", completeFormula "h1" inputs]
          consequence = KIV.And [cmtFormula "h1", KIV.Imp (asmptFormula "h1") (assertionFormula cs "h1" "g")]

-- ===================
-- | PROGRAM --> KIV |
-- ===================

translateProgram :: AC.Program -> [KIV.KIVFile]
translateProgram (AC.Program name channels events instances) = [libraries, channelEvent, instantiateHistory, enrichHistory, system, heuristics] <> instanceSpecs
    where channelEvent = genChannelEvent name channels events
          instantiateHistory = KIV.Instantiate (KIV.SpecName $ progNameToString name ++ "-history-inst") (KIV.SpecName "history") (KIV.SpecName "channel-event") (KIV.SpecName $ progNameToString name ++ "-channel-event")
          enrichHistory = KIV.Enrich historySpec [KIV.SpecName $ progNameToString name ++ "-history-inst"] (KIV.SpecItems [] [] [] [])
          system = KIV.Enrich (KIV.SpecName "system") (KIV.SpecName . tsNameToString <$> instanceNames) (KIV.SpecItems [] [] [] [])
          heuristics = KIV.Heuristics (KIV.SpecName "system") KIV.plCaseSplitHeurSet
          instanceNames = concatMap (\(AC.TransitionSystemInstances _ mappings) -> fst <$> mappings) instances
          instanceSpecs = concatMap (translateTransitionSystemInstances historySpec) instances
          historySpec = KIV.SpecName $ progNameToString name ++ "-history"

libraries = KIV.Libraries [KIV.LibraryImport (KIV.ProjectName "lib-basic") basicImports, KIV.LibraryImport (KIV.ProjectName "lib-async-ac") asyncACImports]
    where basicImports = KIV.SpecName <$> [
                "nat-basic", "nat", "elem",
                "list-data", "list", "list-dup", "list-last", "list-getput", "list-del",
                "set-basic", "set-union"
            ]
          asyncACImports = KIV.SpecName <$> [
                "ctrl-data", "conf",
                "list-util", "set-list",
                "channel-event", "message",
                "channel-set-list-actualize", "channel-set-list",
                "event-set-list-actualize", "event-set-list",
                "message-set-list-actualize", "message-set-list",
                "operation-set-list-actualize", "operation-set-list",
                "history-item", "history"
            ]

genChannelEvent name channels events = KIV.Data (KIV.SpecName $ progNameToString name ++ "-channel-event") [KIV.SpecName "nat"] [channel, event]  -- TODO: all args are nat for now
    where channel = genChannelType channels
          event = genEventType events

genChannelType channels = KIV.DataDefinition channelType constructors
    where constructors = mapSetToList mkConstructor channels
          mkConstructor c = KIV.DataConstructor (KIV.KIVName $ channelToString c) []

genEventType events = KIV.DataDefinition eventType (mapSetToList mkCons events)
    where mkCons (AC.EventDecl evName numArgs) = KIV.DataConstructor (KIV.KIVName $ evNameToString evName) [mkParam i | i <- [1..numArgs]]
          mkParam i = (KIV.KIVName $ "acc" ++ show i, KIV.KIVType "nat") -- TODO: all args are nat for now

translateTransitionSystemInstances :: KIV.SpecName -> AC.TransitionSystemInstances -> [KIV.KIVFile]
translateTransitionSystemInstances historySpec (AC.TransitionSystemInstances ts@(AC.TransitionSystem name ctrlStates _ inputs outputs _ _ _ _ _) mappings) = tsSpecs <> renameSpecs <> channelDefs
    where tsSpecs = translateTransitionSystem historySpec ts
          tsChannels = inputs <> outputs
          renameSpecs = genMappingRename name ctrlStates tsChannels . fst <$> mappings
          channelDefs = uncurry (genMappingChannelDefs tsChannels) <$> mappings

genMappingRename :: AC.TransitionSystemName -> Set.Set AC.ControlState -> Set.Set AC.Channel -> AC.TransitionSystemName -> KIV.KIVFile
genMappingRename name ctrlStates channels newName = KIV.Rename (KIV.SpecName $ tsNameToString newName ++ "-rename") (KIV.SpecName $ tsNameToString name) [] symbolRenames
    where symbolRenames = rename $ [asmpt, cmt, initAssertion] <> mapSetToList assertionName ctrlStates <> mapSetToList channelToString channels
          rename = map (\s -> (KIV.KIVName s, KIV.KIVName $ renameSymbolForMapping newName s))
          
genMappingChannelDefs :: Set.Set AC.Channel -> AC.TransitionSystemName -> AC.ChannelMapping -> KIV.KIVFile
genMappingChannelDefs channels name chanMapping = KIV.Enrich (KIV.SpecName $ tsNameToString name) [KIV.SpecName $ tsNameToString name ++ "-rename"] (KIV.SpecItems [] [] [] axioms)
    where axioms = catMaybes (mapSetToList mkDef channels)
          mkDef c = do
            c' <- chanMapping c
            let renamedC = renameSymbolForMapping name $ channelToString c
            return $ genDefAxiomE renamedC (KIV.OpaqueExpression renamedC) (KIV.OpaqueExpression $ channelToString c')

renameSymbolForMapping name s = s ++ "-" ++ tsNameToString name

genDefAxiomF name toDef def = KIV.Axiom (name ++ "-def") (KIV.Iff toDef def) [KIV.Global, KIV.Local]

genDefAxiomE name toDef def = KIV.Axiom (name ++ "-def") (KIV.Equality toDef def) [KIV.Global, KIV.Local]

progNameToString (AC.ProgramName name) = name
tsNameToString (AC.TransitionSystemName name) = name
channelToString (AC.Channel name) = name
evNameToString (AC.EventName name) = name
varToString (AC.Variable name) = name
controlStateToString (AC.ControlState name) = name

exprToString (AC.OpaqueExpression s) = s
exprToString (AC.VariableExpr var) = varToString var

listToKIVSet items = if null items then "∅" else ("∅ ++ "++) $ intercalate " ++ " items

mapSetToList f = foldr ((:) . f) []
