module UML.CompositeStructure.Semantic (CompositeStructureError(..), readCompositeStructure) where

import Data.Bifunctor (first)
import Data.List (nub)
import Data.Set (fromList, map)

import UML.Common.Types
import UML.CompositeStructure.Types
import UML.CompositeStructure.Parser
    ( CompositeStructureItem(..),
      ParseCompositeStructureError,
      parseCompositeStructure )

data CompositeStructureError =
    SyntaxError ParseCompositeStructureError
    | SemanticError String
    deriving (Show)

readCompositeStructure :: String -> Either CompositeStructureError UMLCompositeStructure
readCompositeStructure description = do
    model <- first SyntaxError $ parseCompositeStructure description
    let (names, components, connections) = items2cstuple model
    let (componentsS, connectionsS) = (fromList components, fromList connections)
    name <- case names of
        [name] -> return name
        _ -> Left $ SemanticError $ "Multiple or missing definitions of name (" ++ show names ++ ")"
    let compositeStructure = UMLCompositeStructure name componentsS connectionsS
    checkDuplicates (components, connections) (Data.Set.map getCompName componentsS, connectionsS)
    checkConnectionComponents compositeStructure
    checkNoPortConnectedTwice compositeStructure
    return compositeStructure

items2cstuple :: [CompositeStructureItem] -> ([StructureName], [Component], [Connection])
items2cstuple = foldr addItem ([], [], [])
    where addItem item (names, components, connections) = 
            case item of
                NameItem n -> (n : names, components, connections)
                ComponentItem c -> (names, c : components, connections)
                ConnectionItem c -> (names, components, c : connections)

checkDuplicates (components, connections) (componentNamesS, connectionsS) =
    lenEqOrLeft components componentNamesS "component names"
    >> lenEqOrLeft connections connectionsS "connections"
    where lenEqOrLeft l s m = if length l == length s then Right () else Left $ SemanticError $ "There are duplicate " ++ m

checkConnectionComponents cs@(UMLCompositeStructure _ _ connections) = mapM_ checkConnection connections
    where checkConnection (Connection (ComponentPort c1 _) (ComponentPort c2 _)) =
            notElemEither c1 componentNames (SemanticError $ "Left component of connection not declared: " ++ show c1)
            >> notElemEither c2 componentNames (SemanticError $ "Right component of connection not declared: " ++ show c2)
            >> if c1 == c2 then Left $ SemanticError "Connecting ports of the same component is not yet supported" else Right ()
          componentNames = getComponentNames cs
          notElemEither x xs err = if x `notElem` xs then Left err else Right ()

checkNoPortConnectedTwice (UMLCompositeStructure _ _ connections) = if null dups then Right () else Left $ SemanticError $ "The following ports are connected twice: " ++ show dups
    where connectedPorts = foldr (:) [] connections >>= \(Connection cp1 cp2) -> [cp1, cp2]
          dups = nub . findDups $ connectedPorts
          findDups [] = []
          findDups (x : xs) = filter (==x) xs ++ findDups xs
