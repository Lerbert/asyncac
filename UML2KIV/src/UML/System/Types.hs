module UML.System.Types where

import Data.Set (Set)

import UML.CompositeStructure.Types
import UML.StateMachine.Types

data UMLSystem = UMLSystem UMLCompositeStructure (Set UMLStateMachine)
    deriving (Eq, Ord, Show)
