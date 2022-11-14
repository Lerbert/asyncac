module UML.CompositeStructure.Types where

import Data.Set (Set, map)

import UML.Common.Types

newtype StructureName = StructureName String
    deriving (Eq, Ord, Show)

newtype ComponentName = ComponentName String
    deriving (Eq, Ord, Show)

data Component = Component {
    getCompName :: ComponentName,
    getCompMachName :: MachineName
}
    deriving (Eq, Ord, Show)

data ComponentPort = ComponentPort ComponentName Port
    deriving (Eq, Ord, Show)

data Connection = Connection ComponentPort ComponentPort
    deriving (Show)

flipConn :: Connection -> Connection
flipConn (Connection cp1 cp2) = Connection cp2 cp1

instance Eq Connection where
    (==) c1@(Connection cp11 cp12) c2@(Connection cp21 cp22)
      | cp11 > cp12 = flipConn c1 == c2
      | cp21 > cp22 = c1 == flipConn c2
      | otherwise = cp11 == cp21 && cp12 == cp22

instance Ord Connection where
    (<=) c1@(Connection cp11 cp12) c2@(Connection cp21 cp22)
      | cp11 > cp12 = flipConn c1 <= c2
      | cp21 > cp22 = c1 <= flipConn c2
      | otherwise = cp11 < cp21 || (cp11 == cp21 && cp12 <= cp22)

data UMLCompositeStructure = UMLCompositeStructure StructureName (Set Component) (Set Connection)
    deriving (Eq, Ord, Show)

getComponentNames :: UMLCompositeStructure -> Set ComponentName
getComponentNames (UMLCompositeStructure _ components _) = Data.Set.map getCompName components

getComponentMachNames :: UMLCompositeStructure -> Set MachineName
getComponentMachNames (UMLCompositeStructure _ components _) = Data.Set.map getCompMachName components
