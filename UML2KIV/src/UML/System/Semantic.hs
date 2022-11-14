module UML.System.Semantic (UMLSystemError(..), checkSystem, makeSystem) where

import Data.Bifunctor (first)
import Data.Set ((\\), fromList, isSubsetOf)
import qualified Data.Set as Set

import UML.System.Types
import UML.CompositeStructure.Types
import UML.StateMachine.Types
import UML.CompositeStructure.Semantic (CompositeStructureError)
import UML.StateMachine.Semantic (StateMachineError)

data UMLSystemError = SemanticError String
    | CompositeStructureError CompositeStructureError
    | MachineError StateMachineError
    deriving (Show)

makeSystem :: Either CompositeStructureError UMLCompositeStructure -> [Either StateMachineError UMLStateMachine] -> Either UMLSystemError UMLSystem
makeSystem cs machines = do
    cs <- first CompositeStructureError cs
    machines <- fromList <$> mapM (first MachineError) machines
    let sys = UMLSystem cs machines
    checkSystem sys
    return sys

checkSystem sys =
    checkMachineNamesUnique sys
    >> checkMachineForComp sys
    >> checkConnectedPortsExist sys

checkMachineNamesUnique (UMLSystem _ machines) = let machineNames = foldr ((:) . name) [] machines in
    if length machineNames == length (fromList machineNames) then Right () else Left $ SemanticError "There are multiple machines with the same name"

checkMachineForComp (UMLSystem compositeStructure machines) = let diff = componentMachNames \\ machineNames in
    if null diff then Right () else Left $ SemanticError $ "Missing machines for " ++ show diff
    where componentMachNames = getComponentMachNames compositeStructure
          machineNames = Set.map name machines

checkConnectedPortsExist (UMLSystem (UMLCompositeStructure _ components connections) machines) = mapM_ checkConnection connections
    where checkConnection (Connection (ComponentPort cname1 port1) (ComponentPort cname2 port2)) = do
            m1 <- okOr (SemanticError $ "Missing machine for " ++ show cname1) $ lookupMachine cname1  -- should not happen, since we check this before
            m2 <- okOr (SemanticError $ "Missing machine for " ++ show cname2) $ lookupMachine cname2  -- should not happen, since we check this before
            let (inputPorts1, outputPorts1) = getDeclaredPorts m1
            let (inputPorts2, outputPorts2) = getDeclaredPorts m2
            trueOr (SemanticError $ "Undeclared port " ++ show port1 ++ " for component " ++ show cname1) $ port1 `elem` inputPorts1 || port1 `elem` outputPorts1
            trueOr (SemanticError $ "Undeclared port " ++ show port2 ++ " for component " ++ show cname2) $ port2 `elem` inputPorts2 || port2 `elem` outputPorts2
            let (inputEvents1, outputEvents1) = getEventsOnPort m1 port1
            let (inputEvents2, outputEvents2) = getEventsOnPort m2 port2
            trueOr (SemanticError $ "Component " ++ show cname1 ++ " declares outputs on " ++ show port1 ++ " that don't match inputs of " ++ show cname2 ++ " on port " ++ show port2 ++ ": " ++ show (outputEvents1 \\ inputEvents2)) $ outputEvents1 `isSubsetOf` inputEvents2
            trueOr (SemanticError $ "Component " ++ show cname2 ++ " declares outputs on " ++ show port2 ++ " that don't match inputs of " ++ show cname1 ++ " on port " ++ show port1 ++ ": " ++ show (outputEvents2 \\ inputEvents1)) $ outputEvents2 `isSubsetOf` inputEvents1
            return ()
          compAssoc = foldr ((:) . \c -> (getCompName c, getCompMachName c)) [] components
          machinesAssoc = foldr ((:) . \m -> (name m, m)) [] machines
          lookupMachine cname = do
            mname <- lookup cname compAssoc
            lookup mname machinesAssoc

getEventsOnPort sm port = (foldMap getEvents (inputDecls sm),  foldMap getEvents (outputDecls sm))
    where getEvents (EventDecl port' evName numArgs)
            | port == port' = Set.singleton (evName, numArgs)
            | otherwise = Set.empty

okOr err (Just a) = Right a
okOr err Nothing = Left err

trueOr err True = Right ()
trueOr err False = Left err
