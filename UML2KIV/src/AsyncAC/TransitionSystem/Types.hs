module AsyncAC.TransitionSystem.Types (module AsyncAC.TransitionSystem.Types, module AsyncAC.Common.Types) where

import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set

import AsyncAC.Common.Types

newtype TransitionSystemName = TransitionSystemName String
    deriving (Eq, Ord, Show)

newtype ControlState = ControlState String
    deriving (Eq, Ord, Show)

type Assertion = (ControlState, Maybe Formula)

data ReceiveDecl = ReceiveDecl Channel EventName [Variable]
    deriving (Eq, Ord, Show)

data SendDecl = SendDecl Channel EventName [Expression]
    deriving (Eq, Ord, Show)

data Assignment = Assignment Variable Expression
    deriving (Eq, Ord, Show)

data Trans = Trans ControlState (Set ReceiveDecl) (Maybe Formula) (Set SendDecl) [Assignment] ControlState
    deriving (Eq, Ord, Show)

isInternal (Trans _ receives _ sends _ _) = Set.null receives && Set.null sends

data TransitionSystem =
    TransitionSystem
        TransitionSystemName -- name
        (Set ControlState) -- ctrlStates
        (Set Variable) -- variables
        (Set Channel) -- inputs
        (Set Channel) -- outputs
        (Set Trans) -- transitions
        ControlState -- initCtrl
        (Maybe Formula) -- initPred
        (Maybe AssumptionCommitmentFormula) -- acProperty
        [Assertion] -- stateAssertions
    deriving (Eq, Ord, Show)

withStateOutputsTransSys :: TransitionSystem -> TransitionSystem
withStateOutputsTransSys = (\(a, _, _) -> a) . withStateOutputsTransSysH

withStateOutputsTransSysH :: TransitionSystem -> (TransitionSystem, Channel, SendDecl)
withStateOutputsTransSysH (TransitionSystem name@(TransitionSystemName nameS) ctrlStates variables inputs outputs transitions initState initPred acp assertions) = (TransitionSystem name ctrlStates variables inputs (Set.insert stateChannel outputs) newTransitions initState initPred acp assertions, stateChannel, stateOutput)
    where stateChannel = Channel "state"
          stateOutput = SendDecl stateChannel (EventName $ nameS ++ "State") $ foldr ((:) . \(Variable name) -> OpaqueExpression name) [] variables
          newTransitions = Set.map addStateOutput transitions
          addStateOutput (Trans src receives guard sends assigns dst) = Trans src receives guard (Set.insert stateOutput sends) assigns dst

instantiate :: TransitionSystem -> TransitionSystemName -> ChannelMapping -> Maybe TransitionSystem
instantiate ts@(TransitionSystem _ ctrlStates variables inputs outputs transitions initState initPred acp assertions) newName chanMapping = do
    newInputs <- setMaybe $ Set.map chanMapping inputs
    newOutputs <- setMaybe $ Set.map chanMapping outputs
    newTransitions <- setMaybe $ Set.map instTrans transitions
    return $ TransitionSystem newName ctrlStates variables newInputs newOutputs newTransitions initState initPred acp assertions
    where instTrans (Trans src rcv guard send assign dst) = do
            newRcv <- setMaybe $ Set.map instRcvDecl rcv
            newSend <- setMaybe $ Set.map instSendDecl send
            return $ Trans src newRcv guard newSend assign dst
          instRcvDecl (ReceiveDecl chan evName args) = do
            newChan <- chanMapping chan
            return $ ReceiveDecl newChan evName args
          instSendDecl (SendDecl chan evName args) = do
            newChan <- chanMapping chan
            return $ SendDecl newChan evName args

setMaybe :: Ord a => Set (Maybe a) -> Maybe (Set a)
setMaybe s = let just = Set.filter isJust s in
    if just == s then return $ Set.map fromJust just else Nothing
