module UML.StateMachine.Semantic (StateMachineError(..), readStateMachine) where

import Data.Set (fromList)
import Data.Maybe (fromMaybe, mapMaybe)

import UML.Common.Types
import UML.StateMachine.Types
import UML.StateMachine.Parser

data StateMachineError =
    SyntaxError ParseMachineError
    | SemanticError String
    deriving (Show)

readStateMachine :: [Char] -> Either StateMachineError UMLStateMachine
readStateMachine description = case parseMachine description of
    Left err -> Left $ SyntaxError err
    Right model -> items2sm $ items2smtuple model
    where items2sm (names, variables, states, inputDecls, outputDecls, initialStates, transitions, acps, assertions) = do
            name <- singletonOrErr (SemanticError $ "Multiple or missing definitions of name (" ++ show names ++ ")") names
            initialState <- singletonOrErr (SemanticError $ "Multiple or missing definitions of initial state (" ++ show initialStates ++ ")") initialStates
            acp <- emptySingletonOrErr (SemanticError $ "Multiple definitions A-C property (" ++ show acps ++ ")") acps
            let lists = (variables, states, inputDecls, outputDecls, transitions)
            let sets@(variablesS, statesS, inputDeclsS, outputDeclsS, transitionsS) = (fromList variables, fromList states, fromList inputDecls, fromList outputDecls, fromList transitions)
            checkDuplicates lists sets
            let machine = UMLStateMachine name variablesS statesS inputDeclsS outputDeclsS initialState transitionsS acp assertions
            checkInitialState machine
            checkTransitions machine
            return machine

items2smtuple :: [MachineItem] -> ([MachineName], [Variable], [MachineState], [EventDecl], [EventDecl], [InitialState], [Trans], [AssumptionCommitmentFormula], [Assertion])
items2smtuple = foldr addItem ([], [], [], [], [], [], [], [], [])
    where addItem item (names, variables, states, inputDecls, outputDecls, initialStates, transitions, acps, assertions) =
            case item of
                NameItem n -> (n : names, variables, states, inputDecls, outputDecls, initialStates, transitions, acps, assertions)
                AcpItem acp -> (names, variables, states, inputDecls, outputDecls, initialStates, transitions, acp : acps, assertions)
                VariableItem v -> (names, v : variables, states, inputDecls, outputDecls, initialStates, transitions, acps, assertions)
                StateItem s a -> (names, variables, s : states, inputDecls, outputDecls, initialStates, transitions, acps, a : assertions)
                InputItem d -> (names, variables, states, d : inputDecls, outputDecls, initialStates, transitions, acps, assertions)
                OutputItem d -> (names, variables, states, inputDecls, d : outputDecls, initialStates, transitions, acps, assertions)
                InitItem i -> (names, variables, states, inputDecls, outputDecls, i : initialStates, transitions, acps, assertions)
                TransItem t -> (names, variables, states, inputDecls, outputDecls, initialStates, t : transitions, acps, assertions)

checkDuplicates (variables, states, inputDecls, outputDecls, transitions) (variablesS, statesS, inputDeclsS, outputDeclsS, transitionsS) =
    lenEqOrLeft variables variablesS "variables"
    >> lenEqOrLeft states statesS "states"
    >> lenEqOrLeft inputDecls inputDeclsS "input declarations"
    >> lenEqOrLeft outputDecls outputDeclsS "output declarations"
    >> lenEqOrLeft transitions transitionsS "transitions"
    where lenEqOrLeft l s m = if length l == length s then Right () else Left $ SemanticError $ "There are duplicate " ++ m

checkInitialState :: UMLStateMachine -> Either StateMachineError ()
checkInitialState (UMLStateMachine _ _ states _ _ (InitialState initState _) _ _ _) = assertElem initState states (SemanticError $ "Initial state not declared as state" ++ show initState)

checkTransitions :: UMLStateMachine -> Either StateMachineError ()
checkTransitions (UMLStateMachine _ variables states inputDecls outputDecls _ transitions _ _) = mapM_ checkTrans transitions
    where checkTrans (Trans src trigger guard dst actions) = 
            assertElem src states (SemanticError $ "Source state not declared as state: " ++ show src)
            >> assertElem dst states (SemanticError $ "Destination state not declared as state: " ++ show dst)
            >> mapM_ checkTrigger trigger
            >> mapM_ checkAction actions
            >> checkDuplicateOutputPorts actions
          checkTrigger (Trigger port evName params) =
            assertElem (EventDecl port evName (length params)) inputDecls (SemanticError $ "Event used for trigger not declared: " ++ show evName ++ " on port " ++ show port ++ " with " ++ show (length params) ++ " argument(s)")
            >> mapM_ (\v -> assertNotElem v variables (SemanticError $ "Variable used in trigger also declared in state: " ++ show v)) params 
          checkAction (AssignAction (Assignment var _)) = assertElem var variables (SemanticError $ "Assigning to undeclared variable: " ++ show var)
          checkAction (OutputAction port evName params) = assertElem (EventDecl port evName (length params)) outputDecls (SemanticError $ "Event used for output not declared: " ++ show evName ++ " on port " ++ show port ++ " with " ++ show (length params) ++ " argument(s)")
          checkDuplicateOutputPorts actions = let outputPorts = mapMaybe getOutputPort actions in
            if length outputPorts == length (fromList outputPorts) then Right () else Left $ SemanticError $ "Multiple output actions on same port not yet supported: " ++ show outputPorts
          getOutputPort (OutputAction port _ _) = Just port
          getOutputPort _ = Nothing
          assertElem x xs err = if x `notElem` xs then Left err else Right ()

singletonOrErr err [x] = Right x
singletonOrErr err _ = Left err

emptySingletonOrErr err [] = Right Nothing
emptySingletonOrErr err [x] = Right $ Just x
emptySingletonOrErr err _ = Left err

assertElem x xs err = trueOrErr err (x `elem` xs)
assertNotElem x xs err = trueOrErr err (x `notElem` xs)
trueOrErr err p = if p then Right () else Left err
