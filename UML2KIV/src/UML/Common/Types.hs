module UML.Common.Types (module UML.Common.Types, module Common.Types) where

import Common.Types

newtype MachineName = MachineName String
    deriving (Eq, Ord, Show)

newtype Port = Port String
    deriving (Eq, Ord, Show)

newtype EventName = EventName String
    deriving (Eq, Ord, Show)
