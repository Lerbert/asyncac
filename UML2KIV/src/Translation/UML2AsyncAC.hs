module Translation.UML2AsyncAC where

import Data.Bifunctor (Bifunctor(first))
import Data.Either (partitionEithers)
import Data.Foldable (find)
import qualified Data.Set as Set

import qualified Common.Types as Common
import qualified AsyncAC.Types as AC
import qualified UML.Types as UML

-- =======================================
-- | STATE MACHINE --> TRANSITION SYSTEM |
-- =======================================

translateStateMachine :: UML.UMLStateMachine -> AC.TransitionSystem
translateStateMachine = translateStateMachineH . UML.makeInputEnabled

translateStateMachineH :: UML.UMLStateMachine -> AC.TransitionSystem
translateStateMachineH (UML.UMLStateMachine name variables states inputDecls outputDecls initialState transitions acp assertions) = AC.TransitionSystem sysName ctrlStates variables inputs outputs acTransitions initState initPred acp sysAssertions
    where sysName = translateMachineName name
          ctrlStates = Set.map translateState states
          inputs = Set.map (translateEventDecl translatePortInput) inputDecls
          outputs = Set.map (translateEventDecl translatePortOutput) outputDecls
          acTransitions = Set.map translateTrans transitions
          (initState, initPred) = translateInitialState initialState
          sysAssertions = map translateAssertion assertions

translateMachineName :: UML.MachineName -> AC.TransitionSystemName
translateMachineName (UML.MachineName n) = AC.TransitionSystemName n

translateState :: UML.MachineState -> AC.ControlState
translateState (UML.MachineState n) = AC.ControlState n

translateEventDecl :: (UML.Port -> AC.Channel) -> UML.EventDecl -> AC.Channel
translateEventDecl translatePort (UML.EventDecl port _ _) = translatePort port

translatePortInput :: UML.Port -> AC.Channel
translatePortInput (UML.Port n) = AC.Channel $ n ++ "I"

translatePortOutput :: UML.Port -> AC.Channel
translatePortOutput (UML.Port n) = AC.Channel $ n ++ "O"

translateTrans :: UML.Trans -> AC.Trans
translateTrans (UML.Trans src trigger guard dst actions) = AC.Trans acSrc receives guard (Set.fromList sends) assigns acDst
    where acSrc = translateState src
          acDst = translateState dst
          receives = maybe Set.empty (Set.singleton . translateTrigger) trigger
          (assigns, sends) = translateActions actions

translateTrigger :: UML.Trigger -> AC.ReceiveDecl
translateTrigger (UML.Trigger port evName args) = AC.ReceiveDecl channel acEvName args
    where channel = translatePortInput port
          acEvName = translateEventName evName

translateEventName :: UML.EventName -> AC.EventName
translateEventName (UML.EventName n) = AC.EventName n

translateActions :: [UML.Action] -> ([AC.Assignment], [AC.SendDecl])
translateActions = partitionEithers . map translateAction

translateAction :: UML.Action -> Either AC.Assignment AC.SendDecl
translateAction (UML.AssignAction assign) = Left $ translateAssignment assign
translateAction (UML.OutputAction port evName args) = Right $ AC.SendDecl channel acEvName args
    where channel = translatePortOutput port
          acEvName = translateEventName evName

translateAssignment :: UML.Assignment -> AC.Assignment
translateAssignment (UML.Assignment var expr) = AC.Assignment var expr

translateInitialState :: UML.InitialState -> (AC.ControlState, Maybe Common.Formula)
translateInitialState (UML.InitialState state pred) = (ctrlState, pred)
    where ctrlState = translateState state

translateAssertion :: UML.Assertion -> AC.Assertion
translateAssertion = first translateState

-- ==========================
-- | UML SYSTEM --> PROGRAM |
-- ==========================

translateSystem :: UML.UMLSystem -> AC.Program
translateSystem (UML.UMLSystem (UML.UMLCompositeStructure name components connections) stateMachines) = AC.Program progName channels events instances
    where progName = translateStructureName name
          events = foldMap genEventDecls stateMachines
          (instances, chans) = unzip $ mapSetToList (\m -> genInstance flippedConnections (machineComponents m) m) stateMachines
          channels = mconcat chans
          flippedConnections = let connList = foldr (:) [] connections in
            connList <> (UML.flipConn <$> connList)
          machineComponents m = mapSetToList UML.getCompName $ Set.filter (\c -> UML.getCompMachName c == UML.name m) components

translateStructureName :: UML.StructureName -> AC.ProgramName
translateStructureName (UML.StructureName name) = AC.ProgramName name

genEventDecls :: UML.UMLStateMachine -> Set.Set AC.EventDecl
genEventDecls sm = Set.map mkDecl (UML.inputDecls sm) <> Set.map mkDecl (UML.outputDecls sm)
    where mkDecl (UML.EventDecl _ evName numArgs) = AC.EventDecl (translateEventName evName) numArgs

genInstance :: [UML.Connection] -> [UML.ComponentName] -> UML.UMLStateMachine -> (AC.TransitionSystemInstances, Set.Set AC.Channel)
genInstance connections cnames stateMachine = let mappings = genMappings <$> cnames in
    let usedChannels = Set.fromList $ snd <$> concat mappings in
    (AC.TransitionSystemInstances (translateStateMachine stateMachine) $ zip (translateComponentName <$> cnames) (flip lookup <$> mappings), usedChannels)
    where genMappings cname = mapSetToList (genInputMapping cname) inputPorts <> mapSetToList (genOutputMapping cname) outputPorts
          genInputMapping cname port = do
            let chan = maybe (genImportChannel cname port) (genDirectedChannel . UML.flipConn) $ lookupLeftConnections connections cname port in
                (translatePortInput port, chan)
          genOutputMapping cname port = do
            let chan = maybe (genExportChannel cname port) genDirectedChannel $ lookupLeftConnections connections cname port in
                (translatePortOutput port, chan)
          (inputPorts, outputPorts) = UML.getDeclaredPorts stateMachine

translateComponentName :: UML.ComponentName -> AC.TransitionSystemName
translateComponentName (UML.ComponentName name) = AC.TransitionSystemName name

lookupLeftConnections :: [UML.Connection] -> UML.ComponentName -> UML.Port -> Maybe UML.Connection
lookupLeftConnections connections cname port = find connMatch connections
    where connMatchH c = connMatchH c || connMatchH (UML.flipConn c)
          connMatch (UML.Connection (UML.ComponentPort cname' port') _) = cname' == cname && port' == port

genDirectedChannel :: UML.Connection -> AC.Channel
genDirectedChannel ((UML.Connection (UML.ComponentPort srcCname srcPort) (UML.ComponentPort dstCname dstPort))) = AC.Channel $ componentNameToString srcCname <> "-" <> portToString srcPort <> "TO" <> componentNameToString dstCname <> "-" <> portToString dstPort

genImportChannel :: UML.ComponentName -> UML.Port -> AC.Channel
genImportChannel cname port = AC.Channel $ componentNameToString cname <> "-" <> portToString port <> "IMPORT"

genExportChannel :: UML.ComponentName -> UML.Port -> AC.Channel
genExportChannel cname port = AC.Channel $ componentNameToString cname <> "-" <> portToString port <> "EXPORT"

componentNameToString :: UML.ComponentName -> String
componentNameToString (UML.ComponentName n) = n

portToString :: UML.Port -> String
portToString (UML.Port p) = p

mapSetToList f = foldr ((:) . f) []
