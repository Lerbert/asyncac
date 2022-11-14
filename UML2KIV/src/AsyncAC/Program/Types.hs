module AsyncAC.Program.Types where

import Data.Set (Set)
import qualified Data.Set as Set

import AsyncAC.Common.Types
import AsyncAC.TransitionSystem.Types

newtype ProgramName = ProgramName String
    deriving (Eq, Ord, Show)

data EventDecl = EventDecl EventName Int
    deriving (Eq, Ord, Show)

data TransitionSystemInstances = TransitionSystemInstances TransitionSystem [(TransitionSystemName, ChannelMapping)]

withStateOutputsInst :: TransitionSystemInstances -> (TransitionSystemInstances, Set Channel, EventDecl)
withStateOutputsInst (TransitionSystemInstances ts@(TransitionSystem _ _ variables _ _ _ _ _ _ _) mappings) = (TransitionSystemInstances newTs newMappings, Set.fromList stateChannels, stateEvent)
    where (newTs, baseStateChannel, SendDecl _ stateEvName _) = withStateOutputsTransSysH ts
          stateEvent = EventDecl stateEvName $ Set.size variables
          (newMappings, stateChannels) = unzip $ map updateMapping mappings
          updateMapping (tsName@(TransitionSystemName name), chanMapping) = let stateChannel = Channel $ name ++ baseStateChanName in
            ((tsName, \c -> if c == baseStateChannel then Just stateChannel else chanMapping c), stateChannel)
          (Channel baseStateChanName) = baseStateChannel

realiseInstances :: TransitionSystemInstances -> Maybe [TransitionSystem]
realiseInstances (TransitionSystemInstances ts mappings) = mapM (uncurry $ instantiate ts) mappings

data Program = Program ProgramName (Set Channel) (Set EventDecl) [TransitionSystemInstances]

withStateOutputs :: Program -> Program
withStateOutputs p@(Program name channels eventDecls instances) = Program name (channels <> Set.unions stateChannels) (eventDecls <> Set.fromList stateEvents) newInstances
    where (newInstances, stateChannels, stateEvents) = unzip3 $ map withStateOutputsInst instances
