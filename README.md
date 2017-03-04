# HaskellDDS

An OpenSplice DDS binding for Haskell, or more precisely, a proof-of-concept binding that is essentially the author’s first significant body of Haskell code and as such is without any pretense of idiomatic or elegant use of the language, and as a proof-of-concept simply ignores the question of what a proper mapping of OMG IDL to Haskell would be, instead opting for an ad-hoc solution that works for the simple cases the author has tried it in.

And yet, despite its flaws and incompleteness, it has proven to be very useful on multiple occasions, and to be in some ways the nicest language binding for DDS the author has used.

It is offered in the hope that it may prove equally useful to others; and should it come to pass that someone well-versed in Haskell finds it of use and has suggestions for improving the quality of the code, then that would be even better.

There is no documentation, except for a few comments in the DDS/Core.hs file and this file.

# Features

The majority of the DDS API is covered:

* Creating participants, topics, publishers, subscribers, writers, and readers;
* Reading and writing data;
* Waiting for data using status conditions or read conditions;
* Support for topic & group coherent data;

with the most obvious omissions being:

* Query conditions;
* Content-filtered topics;
* Listeners.

All of these can be added, though adding listeners is probably not worth the bother, considering the practicable alternative of using a waitset in a separate thread.

Error handling is rather simplistic: some of the operations return a result, some simply `fail` on error. The luxury of a proof-of-concept ... but again, in the author's experience, for most DDS-based application code out there, any errors are treated as fatal anyway.

There is fully integrated support for what the newer DDS specifications call "dynamic types": operating on types not known at compilation time. In this binding, this is done by providing bidirectional conversion (with some caveats) between Aeson’s Value type and the memory representation used by the C binding of OpenSplice (which is itself the standard IDL-to-C mapping). This provides a wealth of functions and operators to access data read from DDS and to construct data to be written in DDS. It also provides easy conversion to/from JSON, and, through Aeson’s support for generics, fully automatic conversion between algebraic data types and DDS samples is easily obtained by providing instances of the ToJSON and FromJSON classes. (None of this is tested on complicated types, but it works with simple ones.)

It should come as no surprise that there is also a quasi-quoter for IDL that will spit out type definitions and that automatically generates instances for the FromJSON and ToJSON classes.

It is also possible to use OpenSplice’s IDL preprocessor `idlpp` for generating the required DDS metadata and to write the Haskell types and conversion functions by hand. This naturally gives better performance but is much more tedious and far more complicated in the build process.

# Platform support

The primary platform is MacOS, but it should work without modifications on Linux as well. On Windows, the Haskell parts should still work, but the `preproc_dds_dcps` script probably won’t. What it does is very simple, and so it should not be a difficult undertaking to get it to work on Windows.

# A few usage notes

Given that the source is a bit disorganised, there are a few notes here to make it easier to get started. Ultimately (after the great eventual rewrite) the source code is intended to be properly documented.

## Functions provided

Please see `src/DDS/Core.hs` — there are comments in there.

## QoS policies

The QoS settings follow a different model than that of the standardized DDS interface. Firstly, all QoS policies are considered as members of a single algebraic data type (and sometimes, this is split out even further than in the standard: for example, the reliability QoS policy is split into “Reliability” and “MaxBlockingTime”). This is particularly noticeable in the treatment of the “DurabilityService” QoS policy, which here is just a list of QoS policies, of which only history, resource limits and service cleanup delay are relevant.

Another significant change to the standard representations is that durations are all specified in seconds as `Double`s. Any negative value or value greater than 2^31-1 is interpreted as infinite.

~~~haskell
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
~~~

QoS policies that do not apply for a particular entity type are simply ignored. The publisher and subscriber QoS are essentially the same and disjunct from the topic and reader/writer QoS, so often one can get away with just a single list of QoS, to be used for all entities.

## Default QoS policies

The default QoS used are the same as those used in the DCPS specification, but with the twist that the default reader (and writer) QoS always inherit from the topic QoS. That is, for a new reader (or writer), the QoS is the “standard” default reader (or writer) QoS, overridden by the topic QoS, and subsequently overridden by the QoS specified as a parameter when creating the reader (or writer). It goes without saying that specifying an empty list of policies means nothing is overridden.

## Read masks

The read and take operations, as well as the read conditions, require specifying a the states of interest. In the C API, this is done by having three bit masks, which is very tedious.

Here the states are specified as a single list, combining the sample, view and instance states of interest, and with the special case of an empty list representing that everything is of interest.

The states are:

* `SState Read` and `SState NotRead`
* `VState New` and `VState NotNew`
* `IState Alive`, `IState NoWriters` and `IState Disposed`

the mapping to the standard should be obvious.

## SampleInfo

The read and take operations all return a list of `(SampleInfo, a)` tuples for a reader of type a, where the SampleInfo is defined as:

~~~haskell
data SampleInfo = SampleInfo
                  { sampleState :: SState
                  , viewState :: VState
                  , instanceState :: IState
                  , validData :: Bool
                  , sourceTime :: Integer
                  , instanceHandle :: Integer
                  , publicationHandle :: Integer
                  , disposedGen :: Integer
                  , noWritersGen :: Integer
                  , sampleRank :: Int
                  , genRank :: Int
                  , absGenRank :: Int
                  , receptionTime :: Integer } deriving (Show)
~~~

These map directly to the DCPS standard (with the exception of receptionTime, which is an OpenSplice extension and means just what the name suggests).

## Timestamps

As can be seen in the `SampleInfo` definition, timestamps are represented as nanoseconds since 1-1-1970 in an Integer. This is also true for timestamps passed to `writeTs`, `writeDisposeTs`, `disposeTs` and `unregisterTs`.

## Type mapping by Quasi-Quoters

The quasi-quoters currently map types as follows:

IDL type           | Haskell type
------------------ | -------------
bool               | Bool
char               | Char
octet              | Word8
short              | Int
unsigned short     | Int
long               | Integer
unsigned long      | Integer
long long          | Integer
unsigned long long | Integer
string             | String
string<N>          | String
sequence<T>        | [T]
sequence<T,N>      | [T]
T[N]               | [T]
enum E { A, B }    | data E = A \| B
struct S { T1 a; T2 b; } | data S = S { sA :: T1, sB :: T2 }
union U switch(E) { case A: T1 a; case B: T2 b; } | data U = Ua T1 \| Ub T2
::Haskell::T       | T

Notes:

* All IDL type names are capitalised in the Haskell type definitions;
* The field names in structs are prefixed, by default with the struct name in lower case, and with the first character of the field name in IDL capitalised;
* Enum symbols all have their first character capitalised, but it has only be tried with upper-case enum symbols;
* Unions support is very limited, the discriminant is nowhere to be found in the generated algebraic data type and all cases should be covered (explicitly or as a default case).
* The `::Haskell::T` trick is useful for referring to Haskell types defined elsewhere (for whatever reason, it could be that the IDL-to-Haskell mapping gets it terribly wrong). The OpenSplice XML meta descriptor that gets generated is not acceptable to OpenSplice, which means `newTopic` will no longer work, but `findTopic` remains possible.

# Building

## Building the Haskell-DDS binding

The author hasn’t quite figured out yet how to elegantly integrate special preprocessing into the Cabal build system, but with OpenSplice’s `release.com` sourced, the following should work:

~~~
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
~~~

The special preprocessing is all done by the “preproc_dds_dcps” script. This preprocesses the “dds_dcps.h” file of OpenSplice to make palatable to c2hs, then runs c2hs on it, taking care to get all the include directories (especially when building against a source tree of OpenSplice, rather than an installed product) needed into the command line.

The author has never tried installing the Haskell-DDS binding, instead opting to run from the source trees.

## Building applications

Building an application is also slightly more involved because the need to link against OpenSplice. To use the binding without installing it:

~~~
ghc --make -threaded -dynamic -fPIC -i$PWD/dist/build -i$PWD/src -L$OSPL_HOME/lib/$SPLICE_TARGET -optl-Wl,-rpath,$OSPL_HOME/lib/$SPLICE_TARGET -ldcpssac -lddskernel Examples/genread
~~~

the “gg” script is a wrapper around ghci that automatically provides the options needed to get OpenSplice linked at run-time.

# Examples

## pong

Pong is an implementation of the “pong” half of ping in the <https://www.github.com/prismtech/opensplice-tools> repository. It continually takes samples and republishes them with the original time stamp in another partition. The full source is given below and plays nicely with `pingpong -s ping test`.

~~~haskell
{-# LANGUAGE DeriveGeneric, TemplateHaskell, QuasiQuotes #-}
import Control.Monad
import DDS

[topicidls|ks|
  struct KeyedSeq {
    unsigned long seq;
    long keyval;
    sequence<octet> baggage;
  };
  #pragma keylist KeyedSeq keyval |]

respond :: TopicClass a => Writer a -> Reader a -> IO ()
respond wr rd = takeAll rd >>= return . map (\(si,d) -> writeTs wr (sourceTime si) d) >>= sequence_

main :: IO ()
main = do
  dp <- newParticipant DefaultDomain
  sub <- newSubscriber [Partition ["test_PING"]] dp
  pub <- newPublisher [Partition ["test_PONG"]] dp
  tp <- newTopic [Reliability Reliable, MaxBlockingTime 1.0] "PingPong" dp :: IO (Topic KeyedSeq)
  rd <- newReader [] tp sub
  wr <- newWriter [] tp pub
  ws <- newWaitsetC [(newReadCondition [] rd, respond wr rd)]
  forever $ wait ws >>= sequence_
~~~

Some things of note:

* “topicidls” is one of the two quasi-quoters (the other is “topicidl”). The one ending in “s” derives an instance of Show, the one without it doesn’t because the automatically derived instances of Show are not always what one desires.
* the “ks|” bit at the very beginning sets a prefix for all the record fields names defined in this block of IDL. The program doesn’t access any fields in the record, but because the fixed prefix, the names will be “ksSeq”, “ksKeyval”, etc. If the prefix weren’t given, the prefix would have been that of the type name in lower case — i.e., “keyedseqSeq”, &c., which is not always to be desired.
* “newWaitsetC” is a convenience function, equivalent to calling newWaitset, then newReadCondition, and finally attaching the read condition.

## sub

Sub is a very limited implementation of one subscription mode of the “pubsub” tool in the <https://www.github.com/prismtech/opensplice-tools> repository: when “pub” is run in throughput testing mode (or at least generating PubSub samples of type KeyedSeq while incrementing the sequence number) publishing in partition “test” — i.e., it plays nicely with `pubsub -m0 -w1 test`.

The peculiar thing about “sub” is that it uses idlpp and a hand-written conversion to Haskell. This makes building it significantly more involved, but at least it shows how to do it.

To build it (assuming preproc_dds_dcps indeed ran idlpp, c2hs and clang/gcc to get the “testtype” library and Examples/KeyedSeq.hs):

~~~
ghc --make -threaded -dynamic -fPIC -idist/build -isrc -L$OSPL_HOME/lib -optl-Wl,-rpath,$OSPL_HOME/lib -ldcpssac -lddskernel -LExamples -optl-Wl,-rpath,$PWD/Examples -ltesttype Examples/sub
~~~

## genread

Genread is an example of a generic reader — it subscribes to DCPSPublication topics, and then automatically creates new readers whenever a new writer for a topic is discovered. It then prints the data in JSON. It shows the use of using the embedded IDL to define a reduced version of a topic type, then use findTopic and read the data, as well as defining a type by hand and making it an instance of the `ToJSON` and `FromJSON` classes.

## cohtest

This is actually a test of OpenSplice behaviour with coherent sets in a transient topic, but it shows the usage of group coherency.
