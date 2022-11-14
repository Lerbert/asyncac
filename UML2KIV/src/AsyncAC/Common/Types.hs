module AsyncAC.Common.Types (module AsyncAC.Common.Types, module Common.Types) where

import Common.Types

newtype EventName = EventName String
    deriving (Eq, Ord, Show)

newtype Channel = Channel String
    deriving (Eq, Ord, Show)

type ChannelMapping = Channel -> Maybe Channel
