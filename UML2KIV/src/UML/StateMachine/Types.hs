module UML.StateMachine.Types (module UML.StateMachine.Types, module UML.Common.Types) where

import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

import UML.Common.Types

-- I/O declaration

data EventDecl = EventDecl Port EventName Int
    deriving (Eq, Ord, Show)

-- states

newtype MachineState = MachineState String
    deriving (Eq, Ord, Show)

data InitialState = InitialState MachineState (Maybe Formula)
    deriving (Eq, Ord, Show)

type Assertion = (MachineState, Maybe Formula)

-- transitions

data Trans = Trans MachineState (Maybe Trigger) (Maybe Formula) MachineState [Action]
    deriving (Eq, Ord, Show)

data Trigger = Trigger Port EventName [Variable]
    deriving (Eq, Ord, Show)

matches :: EventDecl -> Trigger -> Bool
matches (EventDecl port evName _) (Trigger port' evName' _) = port == port' && evName == evName'

data Action = OutputAction Port EventName [Expression]
    | AssignAction Assignment
    deriving (Eq, Ord, Show)

data Assignment = Assignment Variable Expression
    deriving (Eq, Ord, Show)

-- state machine

data UMLStateMachine = UMLStateMachine {
    name :: MachineName,
    variables :: Set Variable,
    states :: Set MachineState,
    inputDecls :: Set EventDecl,
    outputDecls :: Set EventDecl,
    initialState :: InitialState,
    transitions :: Set Trans,
    acProperty :: Maybe AssumptionCommitmentFormula,
    assertions :: [Assertion]
}
    deriving (Eq, Ord, Show)

getDeclaredPorts :: UMLStateMachine -> (Set Port, Set Port)
getDeclaredPorts sm = (Set.map getPort (inputDecls sm),  Set.map getPort (outputDecls sm))
    where getPort (EventDecl port _ _) = port

makeInputEnabled :: UMLStateMachine -> UMLStateMachine
makeInputEnabled sm = sm { transitions = transitions sm <> Set.fromList newTransitions }
    where stateList = foldr (:) [] $ states sm
          inputList = foldr (:) [] $ inputDecls sm
          newTransitions = catMaybes $ do
            state <- stateList
            input <- inputList
            let exitTrans = Set.filter (\(Trans src trigger _ _ _) -> src == state && maybe True (matches input) trigger) $ transitions sm
            -- according to p. 315 of the UML specification (v2.5.1), completion events have dispatching priority
            -- therefore, we include the guards of the completion transitions by using 'maybe True ...' instead of 'maybe False ...' above
            let guards = sequence $ foldr ((:) . (\(Trans _ _ guard _ _) -> guard)) [] exitTrans
            return $ do
                guards <- guards
                let guard = if null guards then Nothing else Just $ Not $ Or guards
                return $ Trans state (Just $ mkTrigger input) guard state []
          mkTrigger (EventDecl port evName numArgs) = Trigger port evName [ Variable ("_x" ++ show i) | i <- [1..numArgs]]
