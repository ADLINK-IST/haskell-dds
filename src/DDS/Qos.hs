module DDS.Qos where

import Data.Word

data DurabilityKind = Volatile | TransientLocal | Transient | Persistent deriving (Enum, Read, Show, Eq, Ord)
data AccessScopeKind = ByInstance | ByTopic | ByGroup deriving (Enum, Read, Show, Eq, Ord)
data OwnershipKind = Shared | Exclusive deriving (Enum, Read, Show, Eq, Ord)
data LivelinessKind = AutomaticLiveliness | ManualByParticipant | ManualByTopic deriving (Enum, Read, Show, Eq, Ord)
data OrderKind = ByReception | BySource deriving (Enum, Read, Show, Eq, Ord)
data HistoryKind = KeepLast Integer | KeepAll deriving (Read, Show, Eq, Ord)
data InvalidSamplesKind = NoInvalid | MinimumInvalid | AllInvalid deriving (Enum, Read, Show, Eq, Ord)
data ReliabilityKind = BestEffort | Reliable | Synchronous deriving (Enum, Read, Show, Eq, Ord)

data QosPolicy = UserData [Word8]
               | TopicData [Word8]
               | GroupData [Word8]
               | Priority Integer
               | Lifespan Double
               | Durability DurabilityKind
               | AccessScope AccessScopeKind
               | CoherentAccess Bool
               | OrderedAccess Bool
               | Deadline Double
               | LatencyBudget Double
               | Ownership OwnershipKind
               | Strength Integer
               | Liveliness LivelinessKind
               | LeaseDuration Double
               | TimeBasedFilter Double
               | Partition [String]
               | Reliability ReliabilityKind
               | MaxBlockingTime Double
               | Order OrderKind
               | History HistoryKind
               | MaxSamples Integer
               | MaxInstances Integer
               | MaxSamplesPerInstance Integer
               | AutoEnable Bool
               | AutoDispose Bool
               | AutoPurgeSuspendedDelay Double
               | AutoUnregisterDelay Double
               | AutoPurgeNoWritersDelay Double
               | AutoPurgeDisposedDelay Double
               | AutoPurgeDisposeAll Bool
               | InvalidSamples InvalidSamplesKind
               | ServiceCleanupDelay Double
               | DurabilityService [QosPolicy]
               deriving (Read, Show, Eq)

type Qos = [QosPolicy]
