{-# LANGUAGE RecordWildCards, InterruptibleFFI #-} {- -*- haskell -*- -}

module DDS.Raw(
  module DDS.Qos,
  Sequence(..), peekSequence, pokeSequence,
  ddsNewString, ddsFree,
  Duration(..),
  Timestamp(..),
  DomainId, defaultDomainId,
  Retcode(..),
  StatusMask(..),
  DomainParticipant, createParticipant, deleteParticipant,
  Subscriber, createSubscriber, deleteSubscriber, getBuiltinSubscriber,
  Publisher, createPublisher, deletePublisher,
  TypeSupport(..), SampleSize(..), TopicClass(..),
  getCurrentTime,
  getTypeName, registerType, freeTypeSupport,
  Topic, createTopic, findTopic, deleteTopic,
  getTopicName, getTopicMetaDescription, getTopicTypeName, getTopicKeyList,
  genericTypeSupportAlloc, registerGenericTypeSupport,
  DataReader, createDataReader, deleteDataReader, lookupDataReader,
  DataWriter, createDataWriter, deleteDataWriter,
  write, writeT, writeDispose, writeDisposeT, dispose, disposeT, unregister, unregisterT,
  SState(..), VState(..), IState(..),
  SampleInfo(..),
  DDS.Raw.read, DDS.Raw.take,
  ConditionClass, Condition,
  Waitset, createWaitset, deleteWaitset, attachCondition, detachCondition, wait,
  GuardCondition, createGuardCondition, deleteGuardCondition, triggerGuardCondition, withGuardCondition,
  ReadCondition, createReadCondition, deleteReadCondition, withReadCondition,
  StatusCondition, withStatusCondition, participantStatusCondition, topicStatusCondition,
  subscriberStatusCondition, publisherStatusCondition,
  dataReaderStatusCondition, dataWriterStatusCondition,
  Status(..), setEnabledStatuses,
  participantStatusChanges, topicStatusChanges,
  subscriberStatusChanges, publisherStatusChanges, dataReaderStatusChanges,
  dataWriterStatusChanges,
  beginCoherent, endCoherent, beginAccess, endAccess,
  enableParticipant, enableTopic, enablePublisher, enableSubscriber, enableDataReader, enableDataWriter,
  waitForHistoricalData,
  getSystemId,
  calcSizeAlign
  ) where

import Data.Int
import Data.Word
import Data.Bits
import Data.List

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Control.Monad
import Control.Exception

import DDS.Qos
import qualified DDS.Type as U

-- Work around __attribute__ specs used on MacOS X for declaring which
-- versions implement what
#define __attribute__(x)

-- Use a mangled dds_dcps.h to replace the "typedef DDS_Object X;"
-- with "typedef struct X *X;", as c2hs seems to have difficulty
-- keeping track of all the void* types that conceptually refer to
-- different types
#include "dds_dcps_mangled.h"

{#context lib = "libdcpssac" prefix = "DDS_"#}

foreign import ccall "DDS_free" ddsFree :: Ptr a -> IO ()
foreign import ccall "DDS_octSeq_allocbuf" ddsNewOctSeqBuf :: Word32 -> IO (Ptr CUChar)
foreign import ccall "DDS_StringSeq_allocbuf" ddsNewStringSeqBuf :: Word32 -> IO (Ptr CString)
foreign import ccall "DDS_string_alloc" ddsNewStringRaw :: Word32 -> IO CString

ddsPeekAndFreeString :: CString -> IO String
ddsPeekAndFreeString p
  | p == nullPtr = return ""
  | otherwise = do
    str <- peekCString p
    ddsFree p
    return str

ddsNewString :: String -> IO CString
ddsNewString str = let n = length str in do
  buf <- ddsNewStringRaw $ fromIntegral n
  pokeArray0 0 buf $ map castCharToCChar str
  return buf

data Sequence a = Sequence {
  seqLength :: Int,
  seqMaximum :: Int,
  seqRelease :: Bool,
  seqBuffer :: Ptr a }
type SequencePtr a = Ptr (Sequence a)

instance Storable (Sequence a) where
  sizeOf _ = {#sizeof DDS_sequence_octet#}
  alignment _ = {#alignof DDS_sequence_octet#}
  peek = peekSequence
  poke = pokeSequence

peekSequence :: Ptr (Sequence a) -> IO (Sequence a)
peekSequence p = do
  xlen <- liftM fromIntegral $ {#get DDS_sequence_octet._length#} p
  xmax <- liftM fromIntegral $ {#get DDS_sequence_octet._maximum#} p
  xrel <- liftM toBool $ {#get DDS_sequence_octet._release#} p
  xbuf <- liftM castPtr $ {#get DDS_sequence_octet._buffer#} p
  return $ Sequence { seqLength = xlen, seqMaximum = xmax, seqRelease = xrel, seqBuffer = xbuf }

pokeSequence :: Ptr (Sequence a) -> Sequence a -> IO ()
pokeSequence p a = do
  {#set DDS_sequence_octet._length#} p $ fromIntegral $ seqLength a
  {#set DDS_sequence_octet._maximum#} p $ fromIntegral $ seqMaximum a
  {#set DDS_sequence_octet._release#} p $ fromBool $ seqRelease a
  {#set DDS_sequence_octet._buffer#} p $ castPtr $ seqBuffer a

peekOctSeq :: SequencePtr CUChar -> IO [Word8]
peekOctSeq p = do
  nseq <- peek p
  value <- peekArray (seqLength nseq) (seqBuffer nseq)
  return $ map fromIntegral value

pokeOctSeq :: SequencePtr CUChar -> [Word8] -> IO ()
pokeOctSeq p value = let n = length value in do
  a <- ddsNewOctSeqBuf $ fromIntegral n
  pokeArray a (map fromIntegral value)
  poke p $ Sequence n n True a

newtype Duration = Duration { durationToDouble :: Double }

instance Storable Duration where
  sizeOf _ = {#sizeof DDS_Duration_t#}
  alignment _ = {#alignof DDS_Duration_t#}
  peek = peekDuration
  poke = pokeDuration

peekDuration :: Ptr Duration -> IO Duration
peekDuration p = do
  sec <- liftM fromIntegral $ {#get DDS_Duration_t.sec#} p :: IO Int32
  nsec <- liftM fromIntegral $ {#get DDS_Duration_t.nanosec#} p :: IO Word32
  if sec == 2147483647 && nsec == 2147483647 then
    do return $ Duration (Prelude.read "Infinity")
  else
    do return $ Duration (fromIntegral sec + (fromIntegral nsec)/1e9)

pokeDuration :: Ptr Duration -> Duration -> IO ()
pokeDuration p (Duration d)
  | d < 0 || d >= 2147483647.0 = do
      {#set DDS_Duration_t.sec#} p 2147483647
      {#set DDS_Duration_t.nanosec#} p 2147483647
  | otherwise = let i = floor d :: Integer in do
      {#set DDS_Duration_t.sec#} p $ fromIntegral i
      {#set DDS_Duration_t.nanosec#} p $ floor ((d - fromIntegral i) * 1e9)

newtype Timestamp = Timestamp { unTimestamp :: Integer } deriving (Show)

instance Storable Timestamp where
  sizeOf _ = {#sizeof DDS_Time_t#}
  alignment _ = {#alignof DDS_Time_t#}
  peek = peekTimestamp
  poke = pokeTimestamp

peekTimestamp :: Ptr Timestamp -> IO Timestamp
peekTimestamp p = do
  s <- liftM fromIntegral $ {#get DDS_Time_t.sec#} p
  ns <- liftM fromIntegral $ {#get DDS_Time_t.nanosec#} p
  return $ Timestamp $ 1000000000 * s + ns

pokeTimestamp :: Ptr Timestamp -> Timestamp -> IO ()
pokeTimestamp p a = do
  {#set DDS_Time_t.sec#} p $ fromIntegral $ s
  {#set DDS_Time_t.nanosec#} p $ fromIntegral $ ns
  where
    ts = unTimestamp a
    s = ts `div` 1000000000
    ns = ts `mod` 1000000000

{#pointer *Duration_t as CDurationPtr#}
{#pointer *TopicQos as TopicQosPtr nocode#}
{#pointer *DomainParticipantQos as DomainParticipantQosPtr nocode#}
{#pointer *PublisherQos as PublisherQosPtr nocode#}
{#pointer *SubscriberQos as SubscriberQosPtr nocode#}
{#pointer *DataWriterQos as DataWriterQosPtr nocode#}
{#pointer *DataReaderQos as DataReaderQosPtr nocode#}
type TopicQosPtr = Ptr TopicQos
type DomainParticipantQosPtr = Ptr DomainParticipantQos
type PublisherQosPtr = Ptr PublisherQos
type SubscriberQosPtr = Ptr SubscriberQos
type DataWriterQosPtr = Ptr WriterQos
type DataReaderQosPtr = Ptr ReaderQos

calcSizeAlign :: U.Type -> (Int, Int) -> (Int, Int)
calcSizeAlign typ (size,align) =
  case typ of
    (U.TInt iw _) ->
      let w = case iw of U.I1 -> 1 ; U.I2 -> 2 ; U.I4 -> 4 ; U.I8 -> 8
      in (pad w size + w, align' w)
    (U.TFloat fw) -> 
      let w = case fw of U.F4 -> 4 ; U.F8 -> 8
      in (pad w size + w, align' w)
    U.TChar -> (size + 1, align)
    U.TBool -> (size + 1, align)
    U.TTime -> (pad 4 size + 8, align' 4)
    (U.TString _) ->
      let a = alignment (undefined :: Ptr ())
          s = sizeOf (undefined :: Ptr ())
      in (pad a size + s, align' a)
    (U.TSequence _ _) ->
      let a = alignment (undefined :: Sequence CChar)
          s = sizeOf (undefined :: Sequence CChar)
      in (pad a size + s, align' a)
    (U.TArray n t) ->
      let (s, a) = calcSizeAlign t (0,1)
      in (pad a size + (fromIntegral n) * s, align' a)
    (U.TStruct ms _) ->
      let (s, a) = foldl' (\acc t -> calcSizeAlign (snd t) acc) (0,1) ms
      in (pad a size + pad a s, align' a)
    (U.TEnum _ _) -> (pad 4 size + 4, align' 4)
    (U.TUnion dt _ cs _ _) ->
      let (sd, ad) = calcSizeAlign dt (0,1)
          sacs = map (\(_,_,t) -> calcSizeAlign t (0,1)) cs
          ssc = maximum $ map fst sacs
          asc = maximum $ map snd sacs
          a = max ad asc
          size' = pad a size + sd
      in (pad asc size' + ssc, align' a)
  where
    align' a = if a > align then a else align
    pad a n = if (n `mod` a) == 0 then n else n + a - (n `mod` a)

type DomainId = Integer

defaultDomainId :: DomainId
defaultDomainId = 2147483647

{#enum define Retcode {
    DDS_RETCODE_OK as RetcodeOk,
    DDS_RETCODE_ERROR as RetcodeError,
    DDS_RETCODE_UNSUPPORTED as RetcodeUnsupported,
    DDS_RETCODE_BAD_PARAMETER as RetcodeBadParameter,
    DDS_RETCODE_PRECONDITION_NOT_MET as RetcodePreconditionNotMet,
    DDS_RETCODE_OUT_OF_RESOURCES as RetcodeOutOfResources,
    DDS_RETCODE_NOT_ENABLED as RetcodeNotEnabled,
    DDS_RETCODE_IMMUTABLE_POLICY as RetcodeImmutable,
    DDS_RETCODE_INCONSISTENT_POLICY as RetcodeInconsistentPolicy,
    DDS_RETCODE_ALREADY_DELETED as RetcodeAlreadyDeleted,
    DDS_RETCODE_TIMEOUT as RetcodeTimeout,
    DDS_RETCODE_NO_DATA as RetcodeNoData,
    DDS_RETCODE_ILLEGAL_OPERATION as RetcodeIllegalOperation } deriving (Eq, Show)#}

convertRetcode :: Retcode -> Either String a
convertRetcode rc = Left (show rc)

{#pointer DomainParticipantFactory as DomainParticipantFactory newtype#}
{#fun pure DomainParticipantFactory_get_instance as domainParticipantFactory {} -> `DomainParticipantFactory'#}

type StatusMask = Integer

{#pointer DomainParticipant as DomainParticipant foreign newtype#}
{#pointer *DomainParticipantListener as DomainParticipantListener#}

foreign import ccall "os_signalHandlerSetEnabled" c_signalHandlerSetEnabled :: CInt -> IO CInt

{#fun DomainParticipantFactory_create_participant as c_createParticipant {`DomainParticipantFactory', fromIntegral `DomainId', `DomainParticipantQosPtr', `DomainParticipantListener', fromIntegral `StatusMask'} -> `DomainParticipant'#}
{#fun DomainParticipantFactory_delete_participant as c_deleteParticipant {`DomainParticipantFactory', `DomainParticipant'} -> `Retcode'#}
{#fun DomainParticipant_delete_contained_entities as c_deleteContainedEntities {`DomainParticipant'} -> `Retcode'#}

createParticipant :: DomainId -> IO (Either String DomainParticipant)
createParticipant dId = do
  _ <- c_signalHandlerSetEnabled 0
  qosp <- c_domainParticipantQosAlloc
  _ <- c_getDefaultDomainParticipantQos domainParticipantFactory qosp
  dp <- c_createParticipant domainParticipantFactory dId qosp nullPtr 0
  result <- withDomainParticipant dp $ \ptr ->
    if ptr == nullPtr then
      return (Left $ "failed to create participant in domain " ++ show dId)
    else do
      return (Right dp)
  ddsFree qosp
  return result

deleteParticipant :: DomainParticipant -> IO Retcode
deleteParticipant dp = do
  _ <- c_deleteContainedEntities dp
  c_deleteParticipant domainParticipantFactory dp

{#fun Entity_get_instance_handle as c_entityInstanceHandle {castPtr `Ptr a'} -> `Word64'#}

-- static void instancehandle_to_id (uint32_t *systemId, uint32_t *localId, DDS_InstanceHandle_t h)
-- {
--   /* Undocumented and unsupported trick */
--   union { struct { uint32_t systemId, localId; } s; DDS_InstanceHandle_t h; } u;
--   u.h = h;
--   *systemId = u.s.systemId & ~0x80000000;
--   *localId = u.s.localId;
-- }
getSystemId :: DomainParticipant -> IO Integer
getSystemId dp = do
  rawid <- withDomainParticipant dp $ \ptr -> c_entityInstanceHandle ptr
  rawsysid <- alloca $ \ptr -> do
    poke ptr rawid
    peek $ castPtr ptr :: IO Word32
  return $ fromIntegral $ rawsysid .&. 0x7fffffff

{#fun DDS_DomainParticipant_get_current_time as c_getCurrentTime {`DomainParticipant', castPtr `Ptr Timestamp'} -> `Retcode'#}

getCurrentTime :: DomainParticipant -> IO Integer
getCurrentTime dp = do
  t <- alloca $ \ptr -> do
    c_getCurrentTime dp ptr
    peek ptr
  return $ unTimestamp t

{#pointer Subscriber as Subscriber foreign newtype#}
{#pointer *SubscriberListener as SubscriberListener#}

{#pointer Publisher as Publisher foreign newtype#}
{#pointer *PublisherListener as PublisherListener#}

{#fun DomainParticipant_create_subscriber as c_createSubscriber {`DomainParticipant', `SubscriberQosPtr', `SubscriberListener', fromIntegral `StatusMask'} -> `Subscriber'#}
{#fun DomainParticipant_delete_subscriber as c_deleteSubscriber {`DomainParticipant', `Subscriber'} -> `Retcode'#}
{#fun Subscriber_delete_contained_entities as c_subscriberDeleteContainedEntities {`Subscriber'} -> `Retcode'#}

{#fun DomainParticipant_create_publisher as c_createPublisher {`DomainParticipant', `PublisherQosPtr', `PublisherListener', fromIntegral `StatusMask'} -> `Publisher'#}
{#fun DomainParticipant_delete_publisher as c_deletePublisher {`DomainParticipant', `Publisher'} -> `Retcode'#}
{#fun Publisher_delete_contained_entities as c_publisherDeleteContainedEntities {`Publisher'} -> `Retcode'#}

createSubscriber :: DomainParticipant -> Qos -> IO (Maybe Subscriber)
createSubscriber dp qos = do
  sub <- withSubscriberQos dp qos $ \qosp -> c_createSubscriber dp qosp nullPtr 0
  withSubscriber sub $ \ptr ->
    if ptr == nullPtr then
      return Nothing
    else
      return (Just sub)

deleteSubscriber :: DomainParticipant -> Subscriber -> IO Retcode
deleteSubscriber dp sub = c_subscriberDeleteContainedEntities sub >> c_deleteSubscriber dp sub

createPublisher :: DomainParticipant -> Qos -> IO (Maybe Publisher)
createPublisher dp qos = do
  pub <- withPublisherQos dp qos $ \qosp -> c_createPublisher dp qosp nullPtr 0
  withPublisher pub $ \ptr ->
    if ptr == nullPtr then
      return Nothing
    else
      return (Just pub)

deletePublisher :: DomainParticipant -> Publisher -> IO Retcode
deletePublisher dp pub = c_publisherDeleteContainedEntities pub >> c_deletePublisher dp pub

{#pointer TypeSupport as RawTypeSupportPtr#}
newtype TypeSupport a = TypeSupport { unTypeSupport :: Ptr (TypeSupport a) }
castTypeSupportPtr :: TypeSupport a -> RawTypeSupportPtr
castTypeSupportPtr = castPtr.unTypeSupport
makeTypeSupport :: RawTypeSupportPtr -> IO (TypeSupport a)
makeTypeSupport ts = return $ TypeSupport $ castPtr ts

{#fun DDS_TypeSupport_register_type as c_typeSupRegisterType {castTypeSupportPtr `TypeSupport a', `DomainParticipant', `String'} -> `Retcode'#}
{#fun DDS_TypeSupport_get_type_name as c_typeSupTypeName {castTypeSupportPtr `TypeSupport a'} -> `String' ddsPeekAndFreeString* #}

newtype SampleSize a = SampleSize { unSampleSize :: Int }

class TopicClass a where
  withSample :: U.TopicType -> a -> (Ptr a -> IO b) -> IO b
  peekSample :: U.TopicType -> Ptr a -> IO a
  sizeofSample :: U.TopicType -> SampleSize a

getTypeName :: TypeSupport a -> IO String
getTypeName ts = c_typeSupTypeName ts

registerType :: TypeSupport a -> DomainParticipant -> IO Retcode
registerType ts dp = do
  tname <- c_typeSupTypeName ts
  ret <- c_typeSupRegisterType ts dp tname
  return ret

freeTypeSupport :: TypeSupport a -> IO ()
freeTypeSupport ts = ddsFree (unTypeSupport ts)

{#pointer Topic as Topic foreign newtype#}
{#pointer TopicDescription as TopicDescription foreign newtype#}
{#pointer *TopicListener as TopicListener#}

{#fun Topic_get_qos as c_getTopicQos {`Topic', `TopicQosPtr'} -> `Retcode'#}
{#fun Publisher_copy_from_topic_qos as c_writerQosCopyFromTopicQos {`Publisher', `DataWriterQosPtr', `TopicQosPtr'} -> `Retcode'#}
{#fun Subscriber_copy_from_topic_qos as c_readerQosCopyFromTopicQos {`Subscriber', `DataReaderQosPtr', `TopicQosPtr'} -> `Retcode'#}

getTopicQos :: Topic -> IO (Either Retcode Qos)
getTopicQos tp = do
  qosp <- c_topicQosAlloc
  rc <- c_getTopicQos tp qosp
  res <- case rc of
    RetcodeOk -> do qos <- peekTopicQos qosp ; return $ Right qos
    _ -> return $ Left rc
  ddsFree qosp
  return res

{#class XTopicDescriptionClass TopicDescription#}
{#class XTopicDescriptionClass => XTopicClass Topic#}

{#fun DomainParticipant_create_topic as c_createTopic {`DomainParticipant', `String', `String', `TopicQosPtr', `TopicListener', fromIntegral `StatusMask'} -> `Topic'#}
{#fun DomainParticipant_find_topic as c_findTopic {`DomainParticipant', `String', castPtr `Ptr Duration'} -> `Topic'#}
{#fun DomainParticipant_delete_topic as c_deleteTopic {`DomainParticipant', `Topic'} -> `Retcode'#}

{#fun DomainParticipant_get_builtin_subscriber as getBuiltinSubscriber {`DomainParticipant'} -> `Subscriber'#}

createTopic :: DomainParticipant -> String -> String -> Qos -> IO (Maybe Topic)
createTopic dp topicName typeName qos = do
  tp <- withTopicQos dp qos $ \qosp -> do
    c_createTopic dp topicName typeName qosp nullPtr 0
  withTopic tp $ \ptr -> if ptr == nullPtr then return Nothing else return (Just tp)

findTopic :: DomainParticipant -> String -> Double -> IO (Maybe Topic)
findTopic dp topicName timeout = alloca $ \durp -> do
  poke durp $ Duration timeout
  tp <- c_findTopic dp topicName durp
  withTopic tp $ \ptr -> if ptr == nullPtr then return Nothing else return (Just tp)

deleteTopic :: DomainParticipant -> Topic -> IO Retcode
deleteTopic = c_deleteTopic

withXTopicDescription_class :: XTopicDescriptionClass p => p -> (Ptr TopicDescription -> IO b) -> IO b
withXTopicDescription_class p = withTopicDescription (topicDescription p)

{#fun TopicDescription_get_name as getTopicName `(XTopicDescriptionClass tp)' => {withXTopicDescription_class* `tp'} -> `String' ddsPeekAndFreeString* #}
{#fun Topic_get_metadescription as getTopicMetaDescription {`Topic'} -> `String' ddsPeekAndFreeString* #}
{#fun Topic_get_keylist as getTopicKeyList {`Topic'} -> `String' ddsPeekAndFreeString* #}
{#fun TopicDescription_get_type_name as getTopicTypeName `(XTopicDescriptionClass tp)' => {withXTopicDescription_class* `tp'} -> `String' ddsPeekAndFreeString* #}

{#fun TypeSupport__alloc as genericTypeSupportAlloc {`String', `String', `String'} -> `TypeSupport a' makeTypeSupport* #}

registerGenericTypeSupport :: DomainParticipant -> String -> String -> String -> IO Retcode
registerGenericTypeSupport dp typeName keyList metaData = do
  ts <- genericTypeSupportAlloc typeName keyList metaData
  ret <- c_typeSupRegisterType ts dp typeName
  freeTypeSupport ts
  return ret

{-
    if ((ts = DDS_TypeSupport__alloc(tn, kl ? kl : "", md)) == NULL)
      error("DDS_TypeSupport__alloc(%s) failed\n", tn);
    if ((result = DDS_TypeSupport_register_type(ts, dp, tn)) != DDS_RETCODE_OK)
      error("DDS_TypeSupport_register_type(%s) failed: %d (%s)\n", tn, (int) result, dds_strerror(result));
    DDS_free(md);
    DDS_free(kl);
    DDS_free(tn);
    DDS_free(ts);
-}

{#pointer DataReader as DataReader foreign newtype#}
{#pointer *DataReaderListener as DataReaderListener#}

{#fun Subscriber_create_datareader as c_createDataReader `(XTopicDescriptionClass tp)' => {`Subscriber', withXTopicDescription_class* `tp', `DataReaderQosPtr', `DataReaderListener', fromIntegral `StatusMask'} -> `DataReader'#}
{#fun Subscriber_delete_datareader as c_deleteDataReader {`Subscriber', `DataReader'} -> `Retcode'#}
{#fun Subscriber_lookup_datareader as c_lookupDataReader {`Subscriber', `String'} -> `DataReader'#}

createDataReader :: Subscriber -> Topic -> Qos -> IO (Maybe DataReader)
createDataReader sub tp qos = do
  dr <- withReaderQos sub tp qos $ \qosp -> c_createDataReader sub tp qosp nullPtr 0
  withDataReader dr $ \ptr -> if ptr == nullPtr then return Nothing else return (Just dr)

deleteDataReader :: Subscriber -> DataReader -> IO Retcode
deleteDataReader = c_deleteDataReader

lookupDataReader :: Subscriber -> String -> IO (Maybe DataReader)
lookupDataReader sub topicName = do
  dr <- c_lookupDataReader sub topicName
  withDataReader dr $ \ptr -> if ptr == nullPtr then return Nothing else return (Just dr)

{#fun Subscriber_begin_access as beginAccess {`Subscriber'} -> `Retcode'#}
{#fun Subscriber_end_access as endAccess {`Subscriber'} -> `Retcode'#}

{#pointer DataWriter as DataWriter foreign newtype#}
{#pointer *DataWriterListener as DataWriterListener#}

-- One wonders why one needs a TopicDescription to create a data
-- reader, but a Topic to create a data writer ... perhaps it has to
-- do with the multi-topics and the content filtered topics.
{#fun Publisher_create_datawriter as c_createDataWriter {`Publisher', `Topic', `DataWriterQosPtr', `DataWriterListener', fromIntegral `StatusMask'} -> `DataWriter'#}
{#fun Publisher_delete_datawriter as c_deleteDataWriter {`Publisher', `DataWriter'} -> `Retcode'#}

createDataWriter :: Publisher -> Topic -> Qos -> IO (Maybe DataWriter)
createDataWriter pub tp qos = do
  dr <- withWriterQos pub tp qos $ \qosp -> c_createDataWriter pub tp qosp nullPtr 0
  withDataWriter dr $ \ptr -> if ptr == nullPtr then return Nothing else return (Just dr)

deleteDataWriter :: Publisher -> DataWriter -> IO Retcode
deleteDataWriter = c_deleteDataWriter

{#fun Publisher_begin_coherent_changes as beginCoherent {`Publisher'} -> `Retcode'#}
{#fun Publisher_end_coherent_changes as endCoherent {`Publisher'} -> `Retcode'#}

{#fun DDS_DataWriter_write as c_write {`DataWriter', castPtr `Ptr ()', fromIntegral `Integer'} -> `Retcode'#}
{#fun DDS_DataWriter_write_w_timestamp as c_writeT {`DataWriter', castPtr `Ptr ()', fromIntegral `Integer', castPtr `Ptr Timestamp'} -> `Retcode'#}
{#fun DDS_DataWriter_writedispose as c_writeDispose {`DataWriter', castPtr `Ptr ()', fromIntegral `Integer'} -> `Retcode'#}
{#fun DDS_DataWriter_writedispose_w_timestamp as c_writeDisposeT {`DataWriter', castPtr `Ptr ()', fromIntegral `Integer', castPtr `Ptr Timestamp'} -> `Retcode'#}
{#fun DDS_DataWriter_dispose as c_dispose {`DataWriter', castPtr `Ptr ()', fromIntegral `Integer'} -> `Retcode'#}
{#fun DDS_DataWriter_dispose_w_timestamp as c_disposeT {`DataWriter', castPtr `Ptr ()', fromIntegral `Integer', castPtr `Ptr Timestamp'} -> `Retcode'#}
{#fun DDS_DataWriter_unregister_instance as c_unregisterInstance {`DataWriter', castPtr `Ptr ()', fromIntegral `Integer'} -> `Retcode'#}
{#fun DDS_DataWriter_unregister_instance_w_timestamp as c_unregisterInstanceT {`DataWriter', castPtr `Ptr ()', fromIntegral `Integer', castPtr `Ptr Timestamp'} -> `Retcode'#}

withTimestamp :: Integer -> (Ptr Timestamp -> IO a) -> IO a
withTimestamp ts f = alloca $ \tsptr -> do { poke tsptr (Timestamp ts) ; f tsptr }

write :: (TopicClass a) => U.TopicType -> DataWriter -> a -> IO Retcode
writeT :: (TopicClass a) => U.TopicType -> DataWriter -> Integer -> a -> IO Retcode
writeDispose :: (TopicClass a) => U.TopicType -> DataWriter -> a -> IO Retcode
writeDisposeT :: (TopicClass a) => U.TopicType -> DataWriter -> Integer -> a -> IO Retcode
dispose :: (TopicClass a) => U.TopicType -> DataWriter -> a -> IO Retcode
disposeT :: (TopicClass a) => U.TopicType -> DataWriter -> Integer -> a -> IO Retcode
unregister :: (TopicClass a) => U.TopicType -> DataWriter -> a -> IO Retcode
unregisterT :: (TopicClass a) => U.TopicType -> DataWriter -> Integer -> a -> IO Retcode

writeLike :: (TopicClass a, Num a1) => (t -> Ptr b1 -> a1 -> IO b) -> U.TopicType -> t -> a -> IO b
writeLikeT :: (TopicClass a, Num a1) => (t -> Ptr b1 -> a1 -> Ptr Timestamp -> IO b) -> U.TopicType -> t -> Integer -> a -> IO b
writeLike f tt wr samp = withSample tt samp $ \ptr -> f wr (castPtr ptr) 0
writeLikeT fT tt wr ts samp = withSample tt samp $ \ptr -> withTimestamp ts $ \tsptr -> fT wr (castPtr ptr) 0 tsptr

write = writeLike c_write
writeT = writeLikeT c_writeT
writeDispose = writeLike c_writeDispose
writeDisposeT = writeLikeT c_writeDisposeT
dispose = writeLike c_dispose
disposeT = writeLikeT c_disposeT
unregister = writeLike c_unregisterInstance
unregisterT = writeLikeT c_unregisterInstanceT

{---------------------------------------------------------------------------------------------}

{#enum define SState {
    DDS_READ_SAMPLE_STATE as Read,
    DDS_NOT_READ_SAMPLE_STATE as NotRead } deriving (Eq, Show)#}
{#enum define VState {
    DDS_NEW_VIEW_STATE as New,
    DDS_NOT_NEW_VIEW_STATE as NotNew } deriving (Eq, Show)#}
{#enum define IState {
    DDS_ALIVE_INSTANCE_STATE as Alive,
    DDS_NOT_ALIVE_DISPOSED_INSTANCE_STATE as Disposed,
    DDS_NOT_ALIVE_NO_WRITERS_INSTANCE_STATE as NoWriters } deriving (Eq, Show)#}

-- FIXME: Timestamps probably better not Integer but something more luxurious
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
{#pointer *SampleInfo as SampleInfoPtr -> SampleInfo#}

-- abusing Storable here: peek is very useful and poke is by definition not needed
instance Storable SampleInfo where
  sizeOf _ = {#sizeof SampleInfo#}
  alignment _ = {#alignof SampleInfo#}
  peek = peekSampleInfo
  poke = error "poke SampleInfo not implemented yet"

peekSampleInfo :: SampleInfoPtr -> IO SampleInfo
peekSampleInfo p = do
  sampleState <- liftM (toEnum.fromIntegral) $ {#get SampleInfo.sample_state#} p
  viewState <- liftM (toEnum.fromIntegral) $ {#get SampleInfo.view_state#} p
  instanceState <- liftM (toEnum.fromIntegral) $ {#get SampleInfo.instance_state#} p
  validData <- liftM toBool $ {#get SampleInfo.valid_data#} p
  stsSec <- liftM fromIntegral $ {#get SampleInfo.source_timestamp.sec#} p
  stsNSec <- liftM fromIntegral $ {#get SampleInfo.source_timestamp.nanosec#} p
  let sourceTime = stsSec * 1000000000 + stsNSec
  instanceHandle <- liftM fromIntegral $ {#get SampleInfo.instance_handle#} p
  publicationHandle <- liftM fromIntegral $ {#get SampleInfo.publication_handle#} p
  disposedGen <- liftM fromIntegral $ {#get SampleInfo.disposed_generation_count#} p
  noWritersGen <- liftM fromIntegral $ {#get SampleInfo.no_writers_generation_count#} p
  sampleRank <- liftM fromIntegral $ {#get SampleInfo.sample_rank#} p
  genRank <- liftM fromIntegral $ {#get SampleInfo.generation_rank#} p
  absGenRank <- liftM fromIntegral $ {#get SampleInfo.absolute_generation_rank#} p
  rtsSec <- liftM fromIntegral $ {#get SampleInfo.reception_timestamp.sec#} p
  rtsNSec <- liftM fromIntegral $ {#get SampleInfo.reception_timestamp.nanosec#} p
  let receptionTime = rtsSec * 1000000000 + rtsNSec
  return $ SampleInfo{..}

foreign import ccall "DDS_SampleInfoSeq__alloc" sampleInfo_allocSequence :: IO (SequencePtr SampleInfo)

{#fun DDS_DataReader_read as c_genDataReaderRead `(TopicClass a)' => {`DataReader', castPtr `SequencePtr a', castPtr `SequencePtr SampleInfo', `Int', `Int', `Int', `Int'} -> `Retcode'#}
{#fun DDS_DataReader_take as c_genDataReaderTake `(TopicClass a)' => {`DataReader', castPtr `SequencePtr a', castPtr `SequencePtr SampleInfo', `Int', `Int', `Int', `Int'} -> `Retcode'#}
{#fun DDS_DataReader_return_loan as c_genDataReaderReturnLoan `(TopicClass a)' => {`DataReader', castPtr `SequencePtr a', castPtr `SequencePtr SampleInfo'} -> `Retcode'#}

-- following peekArray, but with explicit element size
peekSamples :: TopicClass a => U.TopicType -> SampleSize a -> Int -> Ptr a -> IO [a]
peekSamples tt sampleSize size ptr | size <= 0 = return []
                                   | otherwise = f (size-1) []
  where
    ssz = unSampleSize sampleSize
    f 0 acc = do e <- peekSampleOff ptr 0; return (e:acc)
    f n acc = do e <- peekSampleOff ptr n; f (n-1) (e:acc)
    peekSampleOff base idx = peekSample tt $ plusPtr base (idx * ssz)

readTake :: TopicClass a => (DataReader -> SequencePtr a -> SequencePtr SampleInfo -> Int -> Int -> Int -> Int -> IO Retcode) -> U.TopicType -> DataReader -> Int -> Int -> Int -> Int -> IO [(SampleInfo, a)]
readTake oper tt dr maxn sst vst ist = do
  with nullsequence $ \infos -> do
    with nullsequence $ \samples -> do
      Control.Exception.bracket
        (oper dr samples infos maxn' sst vst ist)
        (\_ -> c_genDataReaderReturnLoan dr samples infos)
        (\rc -> case rc of RetcodeOk -> extractSamples samples infos ; _ -> return [])
  where
    maxn' = if maxn <= 0 then (-1) else maxn
    nullsequence = Sequence 0 0 False nullPtr
    extractSamples sptr iptr = do
      xs <- peekSequence $ sptr
      xi <- peekSequence $ iptr
      -- perhaps should unpack s only for indices where i says the data is valid
      -- laziness should help, but I doubt this is lazy considering the
      -- use of the IO monad and the freeing of the buffer
      i <- peekArray (seqLength xi) (seqBuffer xi)
      s <- peekSamples tt (sizeofSample tt) (seqLength xs) (seqBuffer xs)
      return $ zip i s

read :: TopicClass a => U.TopicType -> DataReader -> Int -> Int -> Int -> Int -> IO [(SampleInfo, a)]
take :: TopicClass a => U.TopicType -> DataReader -> Int -> Int -> Int -> Int -> IO [(SampleInfo, a)]
read = readTake c_genDataReaderRead
take = readTake c_genDataReaderTake

{#fun DataReader_wait_for_historical_data as c_waitForHistoricalData {`DataReader', `CDurationPtr'} -> `Retcode'#}
waitForHistoricalData rd tsec tnsec =
  allocaBytes {#sizeof DDS_Duration_t#} $ \durp -> do
    {#set DDS_Duration_t.sec#} durp (fromIntegral tsec)
    {#set DDS_Duration_t.nanosec#} durp (fromIntegral tnsec)
    c_waitForHistoricalData rd durp

{---------------------------------------------------------------------------------------------}

{#pointer WaitSet as WaitsetPtr#}
newtype Waitset = Waitset { unWaitset :: Ptr Waitset }
inWaitset = castPtr.unWaitset
outWaitset p = Waitset $ castPtr p

{#pointer Condition as Condition foreign newtype#}
{#pointer ReadCondition as ReadCondition foreign newtype#}
{#pointer GuardCondition as GuardCondition foreign newtype#}
{#pointer StatusCondition as StatusCondition foreign newtype#}

{#class ConditionClass Condition#}
{#class ConditionClass => GuardConditionClass GuardCondition#}
{#class ConditionClass => ReadConditionClass ReadCondition#}
{#class ConditionClass => StatusConditionClass StatusCondition#}

withCondition_class :: ConditionClass p => p -> (Ptr Condition -> IO b) -> IO b
withCondition_class p = withCondition (condition p)

foreign import ccall "DDS_WaitSet__alloc" c_waitsetAlloc_raw :: IO (Ptr Waitset)

{#fun WaitSet__alloc as c_waitsetAlloc {} -> `Waitset' outWaitset #}
{#fun WaitSet_attach_condition as c_waitsetAttach `(ConditionClass c)' => {inWaitset `Waitset', withCondition_class* `c'} -> `Retcode'#}
{#fun WaitSet_detach_condition as c_waitsetDetach `(ConditionClass c)' => {inWaitset `Waitset', withCondition_class* `c'} -> `Retcode'#}

createWaitset :: IO (Maybe Waitset)
createWaitset = do
  ws <- c_waitsetAlloc
  if inWaitset ws == nullPtr then return Nothing else return $ Just ws

deleteWaitset :: Waitset -> IO ()
deleteWaitset = ddsFree . unWaitset

attachCondition :: ConditionClass c => Waitset -> c -> IO Retcode
detachCondition :: ConditionClass c => Waitset -> c -> IO Retcode
attachCondition = c_waitsetAttach
detachCondition = c_waitsetDetach

foreign import ccall "DDS_ConditionSeq__alloc" c_condition_allocSeq :: IO (SequencePtr (Ptr Condition))
--{#fun WaitSet_wait as c_waitsetWait {inWaitset `Waitset', castPtr `SequencePtr (Ptr Condition)', `CDurationPtr'} -> `Retcode'#}
foreign import ccall interruptible "DDS_WaitSet_wait" c_waitsetWait :: Waitset -> SequencePtr (Ptr Condition) -> CDurationPtr -> IO CInt

-- FIXME: should use Duration
wait :: Waitset -> Integer -> Integer -> IO (Either Retcode [Ptr Condition])
wait ws tsec tnsec =
  allocaBytes {#sizeof DDS_Duration_t#} $ \durp -> do
    {#set DDS_Duration_t.sec#} durp (fromIntegral tsec)
    {#set DDS_Duration_t.nanosec#} durp (fromIntegral tnsec)
    bracket c_condition_allocSeq ddsFree $ \cseqp -> do
      rc <- (toEnum . fromIntegral) <$> c_waitsetWait ws cseqp durp
      case rc of
        RetcodeTimeout -> do return (Right [])
        RetcodeOk -> do
          cseq <- peek cseqp
          cs <- peekArray (seqLength cseq) (seqBuffer cseq)
          return (Right cs)
        _ -> do return (Left rc)

{#fun Condition_get_trigger_value as c_conditionGet `(ConditionClass c)' => {withCondition_class* `c'} -> `Bool'#}

{#fun GuardCondition__alloc as c_guardConditionAlloc {} -> `GuardCondition'#}
{#fun GuardCondition_set_trigger_value as c_guardConditionSetValue {`GuardCondition', `Bool'} -> `Retcode'#}

createGuardCondition :: IO (Maybe GuardCondition)
createGuardCondition = do
  c <- c_guardConditionAlloc
  withGuardCondition c $ \ptr -> if ptr == nullPtr then return Nothing else return (Just c)
deleteGuardCondition :: GuardCondition -> IO ()
deleteGuardCondition c = withGuardCondition c (\p -> ddsFree p)
triggerGuardCondition :: GuardCondition -> IO Retcode
triggerGuardCondition c = c_guardConditionSetValue c True

{#fun DataReader_create_readcondition as c_createReadCondition {`DataReader', `Int', `Int', `Int'} -> `ReadCondition'#}
{#fun DataReader_delete_readcondition as c_deleteReadCondition {`DataReader', `ReadCondition'} -> `Retcode'#}

createReadCondition :: DataReader -> Int -> Int -> Int -> IO (Maybe ReadCondition)
createReadCondition dr sst vst ist = do
  c <- c_createReadCondition dr sst vst ist
  withReadCondition c $ \ptr -> if ptr == nullPtr then return Nothing else return (Just c)
deleteReadCondition :: DataReader -> ReadCondition -> IO Retcode
deleteReadCondition = c_deleteReadCondition

{#enum define Status {
    DDS_INCONSISTENT_TOPIC_STATUS as InconsistentTopic,
    DDS_OFFERED_DEADLINE_MISSED_STATUS as OfferedDeadlineMissed,
    DDS_REQUESTED_DEADLINE_MISSED_STATUS as RequestedDeadlineMissed,
    DDS_OFFERED_INCOMPATIBLE_QOS_STATUS as OfferedIncompatibleQos,
    DDS_REQUESTED_INCOMPATIBLE_QOS_STATUS as RequestedIncompatibleQos,
    DDS_SAMPLE_LOST_STATUS as SampleLost,
    DDS_SAMPLE_REJECTED_STATUS as SampleRejected,
    DDS_DATA_ON_READERS_STATUS as DataOnReaders,
    DDS_DATA_AVAILABLE_STATUS as DataAvailable,
    DDS_LIVELINESS_LOST_STATUS as LivelinessLost,
    DDS_LIVELINESS_CHANGED_STATUS as LivelinessChanged,
    DDS_PUBLICATION_MATCHED_STATUS as PublicationMatched,
    DDS_SUBSCRIPTION_MATCHED_STATUS as SubscriptionMatched } deriving (Eq, Show)#}

-- I can't be bothered to map the full class hierarchy of entities in the Haskell binding ...
{#fun Entity_get_statuscondition as c_getStatusCondition {castPtr `Ptr a'} -> `StatusCondition'#}
{#fun Entity_get_status_changes as c_getStatusChanges {castPtr `Ptr a'} -> `StatusMask' fromIntegral #}
{#fun StatusCondition_set_enabled_statuses as c_setEnabledStatuses {`StatusCondition', fromIntegral `StatusMask'} -> `Retcode'#}
participantStatusCondition :: DomainParticipant -> IO StatusCondition
topicStatusCondition :: Topic -> IO StatusCondition
subscriberStatusCondition :: Subscriber -> IO StatusCondition
publisherStatusCondition :: Publisher -> IO StatusCondition
dataReaderStatusCondition :: DataReader -> IO StatusCondition
dataWriterStatusCondition :: DataWriter -> IO StatusCondition
participantStatusCondition = (flip withDomainParticipant) c_getStatusCondition
topicStatusCondition = (flip withTopic) c_getStatusCondition
subscriberStatusCondition = (flip withSubscriber) c_getStatusCondition
publisherStatusCondition = (flip withPublisher) c_getStatusCondition
dataReaderStatusCondition = (flip withDataReader) c_getStatusCondition
dataWriterStatusCondition = (flip withDataWriter) c_getStatusCondition

setEnabledStatuses :: [Status] -> StatusCondition -> IO Retcode
setEnabledStatuses xs sc = c_setEnabledStatuses sc $ foldl' (.|.) 0 $ map (fromIntegral.fromEnum) xs

statusFromMask :: StatusMask -> [Status]
statusFromMask m = filter f [InconsistentTopic ..]
  where
    f s = (fromIntegral $ fromEnum s) .&. m /= 0

getStatusChanges :: Ptr a -> IO [Status]
getStatusChanges x = c_getStatusChanges x >>= return . statusFromMask

participantStatusChanges :: DomainParticipant -> IO [Status]
topicStatusChanges :: Topic -> IO [Status]
subscriberStatusChanges :: Subscriber -> IO [Status]
publisherStatusChanges :: Publisher -> IO [Status]
dataReaderStatusChanges :: DataReader -> IO [Status]
dataWriterStatusChanges :: DataWriter -> IO [Status]
participantStatusChanges = (flip withDomainParticipant) getStatusChanges
topicStatusChanges = (flip withTopic) getStatusChanges
subscriberStatusChanges = (flip withSubscriber) getStatusChanges
publisherStatusChanges = (flip withPublisher) getStatusChanges
dataReaderStatusChanges = (flip withDataReader) getStatusChanges
dataWriterStatusChanges = (flip withDataWriter) getStatusChanges

{#fun Entity_enable as enableEntity {`Ptr ()'} -> `Retcode'#}
enableParticipant :: DomainParticipant -> IO Retcode
enableParticipant dp = withDomainParticipant dp $ \ptr -> enableEntity (castPtr ptr)
enableTopic :: Topic -> IO Retcode
enableTopic tp = withTopic tp $ \ptr -> enableEntity (castPtr ptr)
enablePublisher :: Publisher -> IO Retcode
enablePublisher pub = withPublisher pub $ \ptr -> enableEntity (castPtr ptr)
enableSubscriber :: Subscriber -> IO Retcode
enableSubscriber sub = withSubscriber sub $ \ptr -> enableEntity (castPtr ptr)
enableDataReader :: DataReader -> IO Retcode
enableDataReader rd = withDataReader rd $ \ptr -> enableEntity (castPtr ptr)
enableDataWriter :: DataWriter -> IO Retcode
enableDataWriter wr = withDataWriter wr $ \ptr -> enableEntity (castPtr ptr)

{-------------------------------------------------------------
---------                                            ---------
---------  QOS                                       ---------
---------                                            ---------
-------------------------------------------------------------}

{#fun TopicQos__alloc as c_topicQosAlloc {} -> `TopicQosPtr'#}
{#fun DomainParticipantQos__alloc as c_domainParticipantQosAlloc {} -> `DomainParticipantQosPtr'#}
{#fun PublisherQos__alloc as c_publisherQosAlloc {} -> `PublisherQosPtr'#}
{#fun SubscriberQos__alloc as c_subscriberQosAlloc {} -> `SubscriberQosPtr'#}
{#fun DataWriterQos__alloc as c_dataWriterQosAlloc {} -> `DataWriterQosPtr'#}
{#fun DataReaderQos__alloc as c_dataReaderQosAlloc {} -> `DataReaderQosPtr'#}

{#fun DomainParticipantFactory_get_default_participant_qos as c_getDefaultDomainParticipantQos {`DomainParticipantFactory', `DomainParticipantQosPtr'} -> `Retcode'#}
{#fun DomainParticipant_get_default_topic_qos as c_getDefaultTopicQos {`DomainParticipant', `TopicQosPtr'} -> `Retcode'#}
{#fun DomainParticipant_get_default_subscriber_qos as c_getDefaultSubscriberQos {`DomainParticipant', `SubscriberQosPtr'} -> `Retcode'#}
{#fun DomainParticipant_get_default_publisher_qos as c_getDefaultPublisherQos {`DomainParticipant', `PublisherQosPtr'} -> `Retcode'#}
{#fun Publisher_get_default_datawriter_qos as c_getDefaultDataWriterQos {`Publisher', `DataWriterQosPtr'} -> `Retcode'#}
{#fun Subscriber_get_default_datareader_qos as c_getDefaultDataReaderQos {`Subscriber', `DataReaderQosPtr'} -> `Retcode'#}

{#enum DurabilityQosPolicyKind_e as DurabilityQosKind {underscoreToCase} deriving (Eq, Ord, Show, Read)#}
{#enum PresentationQosPolicyAccessScopeKind_e as PresentationQosAccessScopeKind {underscoreToCase} deriving (Eq, Ord, Show, Read)#}
{#enum OwnershipQosPolicyKind_e as OwnershipQosKind {underscoreToCase} deriving (Eq, Show, Read)#}
{#enum LivelinessQosPolicyKind_e as LivelinessQosKind {underscoreToCase} deriving (Eq, Ord, Show, Read)#}
{#enum ReliabilityQosPolicyKind_e as ReliabilityQosKind {underscoreToCase} deriving (Eq, Ord, Show, Read)#}
{#enum DestinationOrderQosPolicyKind_e as DestinationOrderQosKind {underscoreToCase} deriving (Eq, Ord, Show, Read)#}
{#enum HistoryQosPolicyKind_e as HistoryQosKind {underscoreToCase} deriving (Eq, Ord, Show, Read)#}
{#enum InvalidSampleVisibilityQosPolicyKind_e as InvalidSampleVisibilityQosKind {underscoreToCase} deriving (Eq, Ord, Show, Read)#}

newtype DomainParticipantQos = DomainParticipantQos Qos deriving (Read, Show, Eq)
newtype TopicQos = TopicQos Qos deriving (Read, Show, Eq)
newtype PublisherQos = PublisherQos Qos deriving (Read, Show, Eq)
newtype SubscriberQos = SubscriberQos Qos deriving (Read, Show, Eq)
newtype WriterQos = WriterQos Qos deriving (Read, Show, Eq)
newtype ReaderQos = ReaderQos Qos deriving (Read, Show, Eq)

peekTopicQos :: Ptr TopicQos -> IO Qos
peekTopicQos ptr =
  let
    ptr' = castPtr ptr :: Ptr QosPolicy
    ftab =
      [(peekTopicData, {#offsetof DDS_TopicQos.topic_data#}),
       (peekDurability, {#offsetof DDS_TopicQos.durability#}),
       (peekDurabilityService, {#offsetof DDS_TopicQos.durability_service#}),
       (peekDeadline, {#offsetof DDS_TopicQos.deadline#}),
       (peekLatencyBudget, {#offsetof DDS_TopicQos.latency_budget#}),
       (peekLiveliness, {#offsetof DDS_TopicQos.liveliness#}),
       (peekLeaseDuration, {#offsetof DDS_TopicQos.liveliness#}),
       (peekReliability, {#offsetof DDS_TopicQos.reliability#}),
       (peekMaxBlockingTime, {#offsetof DDS_TopicQos.reliability#}),
       (peekOrder, {#offsetof DDS_TopicQos.destination_order#}),
       (peekHistory, {#offsetof DDS_TopicQos.history#}),
       (peekMaxSamples, {#offsetof DDS_TopicQos.resource_limits#}),
       (peekMaxInstances, {#offsetof DDS_TopicQos.resource_limits#}),
       (peekMaxSamplesPerInstance, {#offsetof DDS_TopicQos.resource_limits#}),
       (peekPriority, {#offsetof DDS_TopicQos.transport_priority#}),
       (peekLifespan, {#offsetof DDS_TopicQos.lifespan#}),
       (peekOwnership, {#offsetof DDS_TopicQos.ownership#})]
      :: [(Ptr QosPolicy -> IO QosPolicy, Int)]
  in mapM (\(f, off) -> (f (ptr' `plusPtr` off))) ftab

withTopicQos :: DomainParticipant -> Qos -> (Ptr TopicQos -> IO b) -> IO b
withTopicQos dp qos fun = do
    ptr <- c_topicQosAlloc
    _ <- c_getDefaultTopicQos dp ptr
    mapM_ (tr $ castPtr ptr) qos
    result <- fun ptr
    ddsFree ptr
    return result
  where
    tr :: Ptr QosPolicy -> QosPolicy -> IO ()
    tr ptr qp = let o = off qp in do
      if o >= 0 then
        setQosPolicy (ptr `plusPtr` o) qp
      else
        return ()
    off :: QosPolicy -> Int
    off (TopicData _) = {#offsetof DDS_TopicQos.topic_data#}
    off (Durability _) = {#offsetof DDS_TopicQos.durability#}
    off (DurabilityService _) = {#offsetof DDS_TopicQos.durability_service#}
    off (ServiceCleanupDelay _) = {#offsetof DDS_TopicQos.durability_service#}
    off (Deadline _) = {#offsetof DDS_TopicQos.deadline#}
    off (LatencyBudget _) = {#offsetof DDS_TopicQos.latency_budget#}
    off (Liveliness _) = {#offsetof DDS_TopicQos.liveliness#}
    off (LeaseDuration _) = {#offsetof DDS_TopicQos.liveliness#}
    off (Reliability _) = {#offsetof DDS_TopicQos.reliability#}
    off (MaxBlockingTime _) = {#offsetof DDS_TopicQos.reliability#}
    off (Order _) = {#offsetof DDS_TopicQos.destination_order#}
    off (History _) = {#offsetof DDS_TopicQos.history#}
    off (MaxSamples _) = {#offsetof DDS_TopicQos.resource_limits#}
    off (MaxInstances _) = {#offsetof DDS_TopicQos.resource_limits#}
    off (MaxSamplesPerInstance _) = {#offsetof DDS_TopicQos.resource_limits#}
    off (Priority _) = {#offsetof DDS_TopicQos.transport_priority#}
    off (Lifespan _) = {#offsetof DDS_TopicQos.lifespan#}
    off (Ownership _) = {#offsetof DDS_TopicQos.ownership#}
    off _ = -1

peekPublisherQos :: Ptr PublisherQos -> IO Qos
peekPublisherQos ptr =
  let
    ptr' = castPtr ptr :: Ptr QosPolicy
    ftab =
      [(peekAccessScope, {#offsetof DDS_PublisherQos.presentation#}),
       (peekCoherentAccess, {#offsetof DDS_PublisherQos.presentation#}),
       (peekOrderedAccess, {#offsetof DDS_PublisherQos.presentation#}),
       (peekPartition, {#offsetof DDS_PublisherQos.partition#}),
       (peekGroupData, {#offsetof DDS_PublisherQos.group_data#}),
       (peekAutoEnable, {#offsetof DDS_PublisherQos.entity_factory#})]
      :: [(Ptr QosPolicy -> IO QosPolicy, Int)]
  in mapM (\(f, off) -> (f (ptr' `plusPtr` off))) ftab

withPublisherQos :: DomainParticipant -> Qos -> (Ptr PublisherQos -> IO b) -> IO b
withPublisherQos dp qos fun = do
    ptr <- c_publisherQosAlloc
    _ <- c_getDefaultPublisherQos dp ptr
    mapM_ (tr $ castPtr ptr) qos
    result <- fun ptr
    ddsFree ptr
    return result
  where
    tr :: Ptr QosPolicy -> QosPolicy -> IO ()
    tr ptr qp = let o = off qp in do
      if o >= 0 then
        setQosPolicy (ptr `plusPtr` o) qp
      else
        return ()
    off :: QosPolicy -> Int
    off (AccessScope _) = {#offsetof DDS_PublisherQos.presentation#}
    off (CoherentAccess _) = {#offsetof DDS_PublisherQos.presentation#}
    off (OrderedAccess _) = {#offsetof DDS_PublisherQos.presentation#}
    off (Partition _) = {#offsetof DDS_PublisherQos.partition#}
    off (GroupData _) = {#offsetof DDS_PublisherQos.group_data#}
    off (AutoEnable _) = {#offsetof DDS_PublisherQos.entity_factory#}
    off _ = -1

peekSubscriberQos :: Ptr SubscriberQos -> IO Qos
peekSubscriberQos ptr =
  let
    ptr' = castPtr ptr :: Ptr QosPolicy
    ftab =
      [(peekAccessScope, {#offsetof DDS_SubscriberQos.presentation#}),
       (peekCoherentAccess, {#offsetof DDS_SubscriberQos.presentation#}),
       (peekOrderedAccess, {#offsetof DDS_SubscriberQos.presentation#}),
       (peekPartition, {#offsetof DDS_SubscriberQos.partition#}),
       (peekGroupData, {#offsetof DDS_SubscriberQos.group_data#}),
       (peekAutoEnable, {#offsetof DDS_SubscriberQos.entity_factory#})]
      :: [(Ptr QosPolicy -> IO QosPolicy, Int)]
  in mapM (\(f, off) -> (f (ptr' `plusPtr` off))) ftab

withSubscriberQos :: DomainParticipant -> Qos -> (Ptr SubscriberQos -> IO b) -> IO b
withSubscriberQos dp qos fun = do
    ptr <- c_subscriberQosAlloc
    _ <- c_getDefaultSubscriberQos dp ptr
    mapM_ (tr $ castPtr ptr) qos
    result <- fun ptr
    ddsFree ptr
    return result
  where
    tr :: Ptr QosPolicy -> QosPolicy -> IO ()
    tr ptr qp = let o = off qp in do
      if o >= 0 then
        setQosPolicy (ptr `plusPtr` o) qp
      else
        return ()
    off :: QosPolicy -> Int
    off (AccessScope _) = {#offsetof DDS_SubscriberQos.presentation#}
    off (CoherentAccess _) = {#offsetof DDS_SubscriberQos.presentation#}
    off (OrderedAccess _) = {#offsetof DDS_SubscriberQos.presentation#}
    off (Partition _) = {#offsetof DDS_SubscriberQos.partition#}
    off (GroupData _) = {#offsetof DDS_SubscriberQos.group_data#}
    off (AutoEnable _) = {#offsetof DDS_SubscriberQos.entity_factory#}
    off _ = -1

peekWriterQos :: Ptr WriterQos -> IO Qos
peekWriterQos ptr =
  let
    ptr' = castPtr ptr :: Ptr QosPolicy
    ftab =
      [(peekDurability, {#offsetof DDS_DataWriterQos.durability#}),
       (peekDeadline, {#offsetof DDS_DataWriterQos.deadline#}),
       (peekLatencyBudget, {#offsetof DDS_DataWriterQos.latency_budget#}),
       (peekLiveliness, {#offsetof DDS_DataWriterQos.liveliness#}),
       (peekLeaseDuration, {#offsetof DDS_DataWriterQos.liveliness#}),
       (peekReliability, {#offsetof DDS_DataWriterQos.reliability#}),
       (peekMaxBlockingTime, {#offsetof DDS_DataWriterQos.reliability#}),
       (peekOrder, {#offsetof DDS_DataWriterQos.destination_order#}),
       (peekHistory, {#offsetof DDS_DataWriterQos.history#}),
       (peekMaxSamples, {#offsetof DDS_DataWriterQos.resource_limits#}),
       (peekMaxInstances, {#offsetof DDS_DataWriterQos.resource_limits#}),
       (peekMaxSamplesPerInstance, {#offsetof DDS_DataWriterQos.resource_limits#}),
       (peekPriority, {#offsetof DDS_DataWriterQos.transport_priority#}),
       (peekLifespan, {#offsetof DDS_DataWriterQos.lifespan#}),
       (peekUserData, {#offsetof DDS_DataWriterQos.user_data#}),
       (peekOwnership, {#offsetof DDS_DataWriterQos.ownership#}),
       (peekStrength, {#offsetof DDS_DataWriterQos.ownership_strength#}),
       (peekAutoDispose, {#offsetof DDS_DataWriterQos.writer_data_lifecycle#}),
       (peekAutoPurgeSuspendedDelay, {#offsetof DDS_DataWriterQos.writer_data_lifecycle#}),
       (peekAutoUnregisterDelay, {#offsetof DDS_DataWriterQos.writer_data_lifecycle#})]
      :: [(Ptr QosPolicy -> IO QosPolicy, Int)]
  in mapM (\(f, off) -> (f (ptr' `plusPtr` off))) ftab

withWriterQos :: Publisher -> Topic -> Qos -> (Ptr WriterQos -> IO b) -> IO b
withWriterQos pub tp qos fun = do
    ptr <- c_dataWriterQosAlloc
    _ <- c_getDefaultDataWriterQos pub ptr
    tqosptr <- c_topicQosAlloc
    _ <- c_getTopicQos tp tqosptr
    _ <- c_writerQosCopyFromTopicQos pub ptr tqosptr
    ddsFree tqosptr
    mapM_ (tr $ castPtr ptr) qos
    result <- fun ptr
    ddsFree ptr
    return result
  where
    tr :: Ptr QosPolicy -> QosPolicy -> IO ()
    tr ptr qp = let o = off qp in do
      if o >= 0 then
        setQosPolicy (ptr `plusPtr` o) qp
      else
        return ()
    off :: QosPolicy -> Int
    off (Durability _) = {#offsetof DDS_DataWriterQos.durability#}
    off (Deadline _) = {#offsetof DDS_DataWriterQos.deadline#}
    off (LatencyBudget _) = {#offsetof DDS_DataWriterQos.latency_budget#}
    off (Liveliness _) = {#offsetof DDS_DataWriterQos.liveliness#}
    off (LeaseDuration _) = {#offsetof DDS_DataWriterQos.liveliness#}
    off (Reliability _) = {#offsetof DDS_DataWriterQos.reliability#}
    off (MaxBlockingTime _) = {#offsetof DDS_DataWriterQos.reliability#}
    off (Order _) = {#offsetof DDS_DataWriterQos.destination_order#}
    off (History _) = {#offsetof DDS_DataWriterQos.history#}
    off (MaxSamples _) = {#offsetof DDS_DataWriterQos.resource_limits#}
    off (MaxInstances _) = {#offsetof DDS_DataWriterQos.resource_limits#}
    off (MaxSamplesPerInstance _) = {#offsetof DDS_DataWriterQos.resource_limits#}
    off (Priority _) = {#offsetof DDS_DataWriterQos.transport_priority#}
    off (Lifespan _) = {#offsetof DDS_DataWriterQos.lifespan#}
    off (UserData _) = {#offsetof DDS_DataWriterQos.user_data#}
    off (Ownership _) = {#offsetof DDS_DataWriterQos.ownership#}
    off (Strength _) = {#offsetof DDS_DataWriterQos.ownership_strength#}
    off (AutoDispose _) = {#offsetof DDS_DataWriterQos.writer_data_lifecycle#}
    off (AutoPurgeSuspendedDelay _) = {#offsetof DDS_DataWriterQos.writer_data_lifecycle#}
    off (AutoUnregisterDelay _) = {#offsetof DDS_DataWriterQos.writer_data_lifecycle#}
    off _ = -1

peekReaderQos :: Ptr ReaderQos -> IO Qos
peekReaderQos ptr =
  let
    ptr' = castPtr ptr :: Ptr QosPolicy
    ftab =
      [(peekDurability, {#offsetof DDS_DataReaderQos.durability#}),
       (peekDeadline, {#offsetof DDS_DataReaderQos.deadline#}),
       (peekLatencyBudget, {#offsetof DDS_DataReaderQos.latency_budget#}),
       (peekLiveliness, {#offsetof DDS_DataReaderQos.liveliness#}),
       (peekLeaseDuration, {#offsetof DDS_DataReaderQos.liveliness#}),
       (peekReliability, {#offsetof DDS_DataReaderQos.reliability#}),
       (peekMaxBlockingTime, {#offsetof DDS_DataReaderQos.reliability#}),
       (peekOrder, {#offsetof DDS_DataReaderQos.destination_order#}),
       (peekHistory, {#offsetof DDS_DataReaderQos.history#}),
       (peekMaxSamples, {#offsetof DDS_DataReaderQos.resource_limits#}),
       (peekMaxInstances, {#offsetof DDS_DataReaderQos.resource_limits#}),
       (peekMaxSamplesPerInstance, {#offsetof DDS_DataReaderQos.resource_limits#}),
       (peekUserData, {#offsetof DDS_DataReaderQos.user_data#}),
       (peekOwnership, {#offsetof DDS_DataReaderQos.ownership#}),
       (peekTimeBasedFilter, {#offsetof DDS_DataReaderQos.time_based_filter#}),
       (peekAutoPurgeNoWritersDelay, {#offsetof DDS_DataReaderQos.reader_data_lifecycle#}),
       (peekAutoPurgeDisposedDelay, {#offsetof DDS_DataReaderQos.reader_data_lifecycle#}),
       (peekAutoPurgeDisposeAll, {#offsetof DDS_DataReaderQos.reader_data_lifecycle#}),
       (peekInvalidSamples, {#offsetof DDS_DataReaderQos.reader_data_lifecycle#})]
      :: [(Ptr QosPolicy -> IO QosPolicy, Int)]
  in mapM (\(f, off) -> (f (ptr' `plusPtr` off))) ftab

withReaderQos :: Subscriber -> Topic -> Qos -> (Ptr ReaderQos -> IO b) -> IO b
withReaderQos sub tp qos fun = do
    ptr <- c_dataReaderQosAlloc
    _ <- c_getDefaultDataReaderQos sub ptr
    tqosptr <- c_topicQosAlloc
    _ <- c_getTopicQos tp tqosptr
    _ <- c_readerQosCopyFromTopicQos sub ptr tqosptr
    ddsFree tqosptr
    mapM_ (tr $ castPtr ptr) qos
    result <- fun ptr
    ddsFree ptr
    return result
  where
    tr :: Ptr QosPolicy -> QosPolicy -> IO ()
    tr ptr qp = let o = off qp in do
      if o >= 0 then
        setQosPolicy (ptr `plusPtr` o) qp
      else
        return ()
    off :: QosPolicy -> Int
    off (Durability _) = {#offsetof DDS_DataReaderQos.durability#}
    off (Deadline _) = {#offsetof DDS_DataReaderQos.deadline#}
    off (LatencyBudget _) = {#offsetof DDS_DataReaderQos.latency_budget#}
    off (Liveliness _) = {#offsetof DDS_DataReaderQos.liveliness#}
    off (LeaseDuration _) = {#offsetof DDS_DataReaderQos.liveliness#}
    off (Reliability _) = {#offsetof DDS_DataReaderQos.reliability#}
    off (MaxBlockingTime _) = {#offsetof DDS_DataReaderQos.reliability#}
    off (Order _) = {#offsetof DDS_DataReaderQos.destination_order#}
    off (History _) = {#offsetof DDS_DataReaderQos.history#}
    off (MaxSamples _) = {#offsetof DDS_DataReaderQos.resource_limits#}
    off (MaxInstances _) = {#offsetof DDS_DataReaderQos.resource_limits#}
    off (MaxSamplesPerInstance _) = {#offsetof DDS_DataReaderQos.resource_limits#}
    off (UserData _) = {#offsetof DDS_DataReaderQos.user_data#}
    off (Ownership _) = {#offsetof DDS_DataReaderQos.ownership#}
    off (TimeBasedFilter _) = {#offsetof DDS_DataReaderQos.time_based_filter#}
    off (AutoPurgeNoWritersDelay _) = {#offsetof DDS_DataReaderQos.reader_data_lifecycle#}
    off (AutoPurgeDisposedDelay _) = {#offsetof DDS_DataReaderQos.reader_data_lifecycle#}
    off (AutoPurgeDisposeAll _) = {#offsetof DDS_DataReaderQos.reader_data_lifecycle#}
    off (InvalidSamples _) = {#offsetof DDS_DataReaderQos.reader_data_lifecycle#}
    off _ = -1

peekTopicData :: Ptr QosPolicy -> IO QosPolicy
peekTopicData p = do
  v <- peekOctSeq (castPtr $ p `plusPtr` {#offsetof DDS_TopicDataQosPolicy.value#})
  return $ TopicData v
peekGroupData :: Ptr QosPolicy -> IO QosPolicy
peekGroupData p = do
  v <- peekOctSeq (castPtr $ p `plusPtr` {#offsetof DDS_GroupDataQosPolicy.value#})
  return $ GroupData v
peekUserData :: Ptr QosPolicy -> IO QosPolicy
peekUserData p = do
  v <- peekOctSeq (castPtr $ p `plusPtr` {#offsetof DDS_UserDataQosPolicy.value#})
  return $ UserData v
peekDurability :: Ptr QosPolicy -> IO QosPolicy
peekDurability p = do
  v <- liftM (toEnum.fromIntegral) $ {#get DDS_DurabilityQosPolicy.kind#} p
  case v of
    VolatileDurabilityQos -> return $ Durability Volatile
    TransientLocalDurabilityQos -> return $ Durability TransientLocal
    TransientDurabilityQos -> return $ Durability Transient
    PersistentDurabilityQos -> return $ Durability Persistent
peekDurabilityService :: Ptr QosPolicy -> IO QosPolicy
peekDurabilityService p = do
  scd  <- peekByteOff p {#offsetof DDS_DurabilityServiceQosPolicy.service_cleanup_delay#}
  kind <- liftM (toEnum.fromIntegral) $ {#get DDS_DurabilityServiceQosPolicy.history_kind#} p
  hist <- case kind of
    KeepAllHistoryQos ->
      return $ History KeepAll
    KeepLastHistoryQos -> do
      depth <- liftM fromIntegral $ {#get DDS_DurabilityServiceQosPolicy.history_depth#} p
      return $ History (KeepLast depth)
  ms   <- liftM fromIntegral $ {#get DDS_DurabilityServiceQosPolicy.max_samples#} p
  mi   <- liftM fromIntegral $ {#get DDS_DurabilityServiceQosPolicy.max_instances#} p
  msi  <- liftM fromIntegral $ {#get DDS_DurabilityServiceQosPolicy.max_samples_per_instance#} p
  return $ DurabilityService [ServiceCleanupDelay scd, hist, MaxSamples ms, MaxInstances mi, MaxSamplesPerInstance msi]
peekAccessScope :: Ptr QosPolicy -> IO QosPolicy
peekAccessScope p = do
  v <- liftM (toEnum.fromIntegral) $ {#get DDS_PresentationQosPolicy.access_scope#} p
  case v of
    InstancePresentationQos -> return $ AccessScope ByInstance
    TopicPresentationQos -> return $ AccessScope ByTopic
    GroupPresentationQos -> return $ AccessScope ByGroup
peekCoherentAccess :: Ptr QosPolicy -> IO QosPolicy
peekCoherentAccess p = do
  v <- {#get DDS_PresentationQosPolicy.coherent_access#} p
  return $ CoherentAccess (v /= 0)
peekOrderedAccess :: Ptr QosPolicy -> IO QosPolicy
peekOrderedAccess p = do
  v <- {#get DDS_PresentationQosPolicy.ordered_access#} p
  return $ OrderedAccess (v /= 0)
peekDeadline :: Ptr QosPolicy -> IO QosPolicy
peekDeadline p = do
  v <- liftM (durationToDouble) $ peekByteOff p {#offsetof DDS_DeadlineQosPolicy.period#}
  return $ Deadline v
peekLatencyBudget :: Ptr QosPolicy -> IO QosPolicy
peekLatencyBudget p = do
  v <- liftM (durationToDouble) $ peekByteOff p {#offsetof DDS_LatencyBudgetQosPolicy.duration#}
  return $ LatencyBudget v
peekLiveliness :: Ptr QosPolicy -> IO QosPolicy
peekLiveliness p = do
  v <- liftM (toEnum.fromIntegral) $ {#get DDS_LivelinessQosPolicy.kind#} p
  case v of
    AutomaticLivelinessQos -> return $ Liveliness AutomaticLiveliness
    ManualByParticipantLivelinessQos -> return $ Liveliness ManualByParticipant
    ManualByTopicLivelinessQos -> return $ Liveliness ManualByTopic
peekLeaseDuration :: Ptr QosPolicy -> IO QosPolicy
peekLeaseDuration p = do
  v <- liftM (durationToDouble) $ peekByteOff p {#offsetof DDS_LivelinessQosPolicy.lease_duration#}
  return $ LeaseDuration v
peekReliability :: Ptr QosPolicy -> IO QosPolicy
peekReliability p = do
  v <- liftM (toEnum.fromIntegral) $ {#get DDS_ReliabilityQosPolicy.kind#} p
  case v of
    BestEffortReliabilityQos -> return $ Reliability BestEffort
    ReliableReliabilityQos -> do
      w <- {#get DDS_ReliabilityQosPolicy.synchronous#} p
      case w of
        0 -> return $ Reliability Reliable
        _ -> return $ Reliability Synchronous
peekMaxBlockingTime :: Ptr QosPolicy -> IO QosPolicy
peekMaxBlockingTime p = do
  v <- liftM (durationToDouble) $ peekByteOff p {#offsetof DDS_ReliabilityQosPolicy.max_blocking_time#}
  return $ MaxBlockingTime v
peekOrder :: Ptr QosPolicy -> IO QosPolicy
peekOrder p = do
  v <- liftM (toEnum.fromIntegral) $ {#get DDS_DestinationOrderQosPolicy.kind#} p
  case v of
    ByReceptionTimestampDestinationorderQos -> return $ Order ByReception
    BySourceTimestampDestinationorderQos -> return $ Order BySource
peekHistory :: Ptr QosPolicy -> IO QosPolicy
peekHistory p = do
  kind <- liftM (toEnum.fromIntegral) $ {#get DDS_HistoryQosPolicy.kind#} p
  case kind of
    KeepAllHistoryQos ->
      return $ History KeepAll
    KeepLastHistoryQos -> do
      depth <- liftM fromIntegral $ {#get DDS_HistoryQosPolicy.depth#} p
      return $ History (KeepLast depth)
peekMaxSamples :: Ptr QosPolicy -> IO QosPolicy
peekMaxSamples p = do
  v <- liftM fromIntegral $ {#get DDS_ResourceLimitsQosPolicy.max_samples#} p
  return $ MaxSamples v
peekMaxInstances :: Ptr QosPolicy -> IO QosPolicy
peekMaxInstances p = do
  v <- liftM fromIntegral $ {#get DDS_ResourceLimitsQosPolicy.max_instances#} p
  return $ MaxInstances v
peekMaxSamplesPerInstance :: Ptr QosPolicy -> IO QosPolicy
peekMaxSamplesPerInstance p = do
  v <- liftM fromIntegral $ {#get DDS_ResourceLimitsQosPolicy.max_samples_per_instance#} p
  return $ MaxSamplesPerInstance v
peekPriority :: Ptr QosPolicy -> IO QosPolicy
peekPriority p = do
  v <- liftM fromIntegral $ {#get DDS_TransportPriorityQosPolicy.value#} p
  return $ Priority v
peekLifespan :: Ptr QosPolicy -> IO QosPolicy
peekLifespan p = do
  v <- liftM (durationToDouble) $ peekByteOff p {#offsetof DDS_LifespanQosPolicy.duration#}
  return $ Lifespan v
peekOwnership :: Ptr QosPolicy -> IO QosPolicy
peekOwnership p = do
  v <- liftM (toEnum.fromIntegral) $ {#get DDS_OwnershipQosPolicy.kind#} p
  case v of
    SharedOwnershipQos -> return $ Ownership Shared
    ExclusiveOwnershipQos -> return $ Ownership Exclusive
peekStrength :: Ptr QosPolicy -> IO QosPolicy
peekStrength p = do
  v <- liftM fromIntegral $ {#get DDS_OwnershipStrengthQosPolicy.value#} p
  return $ Strength v
peekAutoEnable :: Ptr QosPolicy -> IO QosPolicy
peekAutoEnable p = do
  v <- {#get DDS_EntityFactoryQosPolicy.autoenable_created_entities#} p
  return $ AutoEnable (v /= 0)
peekTimeBasedFilter :: Ptr QosPolicy -> IO QosPolicy
peekTimeBasedFilter p = do
  v <- liftM (durationToDouble) $ peekByteOff p {#offsetof DDS_TimeBasedFilterQosPolicy.minimum_separation#}
  return $ TimeBasedFilter v
peekPartition :: Ptr QosPolicy -> IO QosPolicy
peekPartition p = do
  nseq <- peekByteOff p {#offsetof DDS_PartitionQosPolicy.name#} :: IO (Sequence CString)
  ps' <- peekArray (seqLength nseq) (seqBuffer nseq) :: IO [CString]
  ps <- mapM peekCString ps' :: IO [String]
  return $ Partition ps
peekAutoDispose :: Ptr QosPolicy -> IO QosPolicy
peekAutoDispose p = do
  v <- {#get DDS_WriterDataLifecycleQosPolicy.autodispose_unregistered_instances#} p
  return $ AutoDispose (v /= 0)
peekAutoPurgeSuspendedDelay :: Ptr QosPolicy -> IO QosPolicy
peekAutoPurgeSuspendedDelay p = do
  v <- liftM (durationToDouble) $ peekByteOff p {#offsetof DDS_WriterDataLifecycleQosPolicy.autopurge_suspended_samples_delay#}
  return $ AutoPurgeSuspendedDelay v
peekAutoUnregisterDelay :: Ptr QosPolicy -> IO QosPolicy
peekAutoUnregisterDelay p = do
  v <- liftM (durationToDouble) $ peekByteOff p {#offsetof DDS_WriterDataLifecycleQosPolicy.autounregister_instance_delay#}
  return $ AutoUnregisterDelay v
peekAutoPurgeNoWritersDelay :: Ptr QosPolicy -> IO QosPolicy
peekAutoPurgeNoWritersDelay p = do
  v <- liftM (durationToDouble) $ peekByteOff p {#offsetof DDS_ReaderDataLifecycleQosPolicy.autopurge_nowriter_samples_delay#}
  return $ AutoPurgeNoWritersDelay v
peekAutoPurgeDisposedDelay :: Ptr QosPolicy -> IO QosPolicy
peekAutoPurgeDisposedDelay p = do
  v <- liftM (durationToDouble) $ peekByteOff p {#offsetof DDS_ReaderDataLifecycleQosPolicy.autopurge_disposed_samples_delay#}
  return $ AutoPurgeDisposedDelay v
peekAutoPurgeDisposeAll :: Ptr QosPolicy -> IO QosPolicy
peekAutoPurgeDisposeAll p = do
  v <- {#get DDS_ReaderDataLifecycleQosPolicy.autopurge_dispose_all#} p
  return $ AutoPurgeDisposeAll (v /= 0)
peekInvalidSamples :: Ptr QosPolicy -> IO QosPolicy
peekInvalidSamples p = do
  v <- liftM (toEnum.fromIntegral) $ {#get DDS_ReaderDataLifecycleQosPolicy.invalid_sample_visibility.kind#} p
  case v of
    NoInvalidSamples -> return $ InvalidSamples NoInvalid
    MinimumInvalidSamples -> return $ InvalidSamples MinimumInvalid
    AllInvalidSamples -> return $ InvalidSamples AllInvalid

setQosPolicy :: Ptr QosPolicy -> QosPolicy -> IO ()
setQosPolicy p (UserData value) = do
  old <- {#get DDS_UserDataQosPolicy.value._buffer#} p
  ddsFree old
  pokeOctSeq (castPtr $ p `plusPtr` {#offsetof DDS_UserDataQosPolicy.value#}) value
setQosPolicy p (TopicData value) = do
  old <- {#get DDS_TopicDataQosPolicy.value._buffer#} p
  ddsFree old
  pokeOctSeq (castPtr $ p `plusPtr` {#offsetof DDS_TopicDataQosPolicy.value#}) value
setQosPolicy p (GroupData value) = do
  old <- {#get DDS_GroupDataQosPolicy.value._buffer#} p
  ddsFree old
  pokeOctSeq (castPtr $ p `plusPtr` {#offsetof DDS_GroupDataQosPolicy.value#}) value
setQosPolicy p (Priority value) =
  {#set DDS_TransportPriorityQosPolicy.value#} p $ fromIntegral value
setQosPolicy p (Lifespan duration) =
  pokeByteOff p {#offsetof DDS_LifespanQosPolicy.duration#} $ Duration duration
setQosPolicy p (Durability kind) =
  {#set DDS_DurabilityQosPolicy.kind#} p $ (fromIntegral.fromEnum) kind
setQosPolicy p (AccessScope k) =
  {#set DDS_PresentationQosPolicy.access_scope#} p $ (fromIntegral.fromEnum) k'
  where k' = case k of
          ByInstance -> InstancePresentationQos
          ByTopic -> TopicPresentationQos
          ByGroup -> GroupPresentationQos
setQosPolicy p (CoherentAccess k) =
  {#set DDS_PresentationQosPolicy.coherent_access#} p $ fromBool k
setQosPolicy p (OrderedAccess k) =
  {#set DDS_PresentationQosPolicy.ordered_access#} p $ fromBool k
setQosPolicy p (Deadline period) =
  pokeByteOff p {#offsetof DDS_DeadlineQosPolicy.period#} $ Duration period
setQosPolicy p (LatencyBudget duration) =
  pokeByteOff p {#offsetof DDS_LatencyBudgetQosPolicy.duration#} $ Duration duration
setQosPolicy p (Ownership k) =
  {#set DDS_OwnershipQosPolicy.kind#} p $ (fromIntegral.fromEnum) k'
  where k' = case k of
          Shared -> SharedOwnershipQos
          Exclusive -> ExclusiveOwnershipQos
setQosPolicy p (Strength value) =
  {#set DDS_OwnershipStrengthQosPolicy.value#} p $ fromIntegral value
setQosPolicy p (Liveliness k) =
  {#set DDS_LivelinessQosPolicy.kind#} p $ (fromIntegral.fromEnum) k'
  where k' = case k of
          AutomaticLiveliness -> AutomaticLivelinessQos
          ManualByParticipant -> ManualByParticipantLivelinessQos
          ManualByTopic -> ManualByTopicLivelinessQos
setQosPolicy p (LeaseDuration k) =
  pokeByteOff p {#offsetof DDS_LivelinessQosPolicy.lease_duration#} $ Duration k
setQosPolicy p (TimeBasedFilter ms) =
  pokeByteOff p {#offsetof DDS_TimeBasedFilterQosPolicy.minimum_separation#} $ Duration ms
setQosPolicy p (Partition ps) = let n = length ps in do
  ps' <- mapM ddsNewString ps
  buf <- ddsNewStringSeqBuf $ fromIntegral n
  pokeArray (castPtr buf) ps'
  let nseq = Sequence { seqLength = n, seqMaximum = n, seqRelease = True, seqBuffer = buf }
  old <- {#get DDS_PartitionQosPolicy.name._buffer#} p
  ddsFree old
  pokeByteOff p {#offsetof DDS_PartitionQosPolicy.name#} nseq
setQosPolicy p (Reliability k) = do
  {#set DDS_ReliabilityQosPolicy.kind#} p $ (fromIntegral.fromEnum) k'
  {#set DDS_ReliabilityQosPolicy.synchronous#} p $ fromBool sync
  where (k', sync) = case k of
          BestEffort  -> (BestEffortReliabilityQos, False)
          Reliable    -> (ReliableReliabilityQos, False)
          Synchronous -> (ReliableReliabilityQos, True)
setQosPolicy p (MaxBlockingTime k) =
  pokeByteOff p {#offsetof DDS_ReliabilityQosPolicy.max_blocking_time#} $ Duration k
setQosPolicy p (Order k) =
  {#set DDS_DestinationOrderQosPolicy.kind#} p $ (fromIntegral.fromEnum) k'
  where k' = case k of
          ByReception -> ByReceptionTimestampDestinationorderQos
          BySource -> BySourceTimestampDestinationorderQos
setQosPolicy p (History KeepAll) = do
  {#set DDS_HistoryQosPolicy.kind#} p $ (fromIntegral.fromEnum) KeepAllHistoryQos
  {#set DDS_HistoryQosPolicy.depth#} p $ (1::CInt)
setQosPolicy p (History (KeepLast n)) = do
  {#set DDS_HistoryQosPolicy.kind#} p $ (fromIntegral.fromEnum) KeepLastHistoryQos
  {#set DDS_HistoryQosPolicy.depth#} p $ fromIntegral n
setQosPolicy p (MaxSamples ms) =
  {#set DDS_ResourceLimitsQosPolicy.max_samples#} p $ fromIntegral ms
setQosPolicy p (MaxInstances mi) =
  {#set DDS_ResourceLimitsQosPolicy.max_instances#} p $ fromIntegral mi
setQosPolicy p (MaxSamplesPerInstance mspi) =
  {#set DDS_ResourceLimitsQosPolicy.max_samples_per_instance#} p $ fromIntegral mspi
setQosPolicy p (AutoEnable k) =
  {#set DDS_EntityFactoryQosPolicy.autoenable_created_entities#} p $ fromBool k
setQosPolicy p (AutoDispose k) =
  {#set DDS_WriterDataLifecycleQosPolicy.autodispose_unregistered_instances#} p $ fromBool k
setQosPolicy p (AutoPurgeSuspendedDelay k) =
  pokeByteOff p {#offsetof DDS_WriterDataLifecycleQosPolicy.autopurge_suspended_samples_delay#} $ Duration k
setQosPolicy p (AutoUnregisterDelay k) =
  pokeByteOff p {#offsetof DDS_WriterDataLifecycleQosPolicy.autounregister_instance_delay#} $ Duration k
setQosPolicy p (AutoPurgeNoWritersDelay k) =
  pokeByteOff p {#offsetof DDS_ReaderDataLifecycleQosPolicy.autopurge_nowriter_samples_delay#} $ Duration k
setQosPolicy p (AutoPurgeDisposedDelay k) =
  pokeByteOff p {#offsetof DDS_ReaderDataLifecycleQosPolicy.autopurge_disposed_samples_delay#} $ Duration k
setQosPolicy p (AutoPurgeDisposeAll k) =
  {#set DDS_ReaderDataLifecycleQosPolicy.autopurge_dispose_all#} p $ fromBool k
setQosPolicy p (InvalidSamples k) = do
  {#set DDS_ReaderDataLifecycleQosPolicy.enable_invalid_samples#} p $ fromBool eis
  {#set DDS_ReaderDataLifecycleQosPolicy.invalid_sample_visibility.kind#} p $ (fromIntegral.fromEnum) k'
  where (k', eis) = case k of
          NoInvalid      -> (NoInvalidSamples, False)
          MinimumInvalid -> (MinimumInvalidSamples, True)
          AllInvalid     -> (AllInvalidSamples, True)
setQosPolicy p (ServiceCleanupDelay k) =
  pokeByteOff p {#offsetof DDS_DurabilityServiceQosPolicy.service_cleanup_delay#} $ Duration k
setQosPolicy p (DurabilityService qs) = mapM_ pk qs
  where
    pk :: QosPolicy -> IO ()
    pk (ServiceCleanupDelay k) =
      pokeByteOff p {#offsetof DDS_DurabilityServiceQosPolicy.service_cleanup_delay#} $ Duration k
    pk (History KeepAll) = do
      {#set DDS_DurabilityServiceQosPolicy.history_kind#} p $ (fromIntegral.fromEnum) KeepAllHistoryQos
      {#set DDS_DurabilityServiceQosPolicy.history_depth#} p $ fromIntegral (1::Int)
    pk (History (KeepLast n)) = do
      {#set DDS_DurabilityServiceQosPolicy.history_kind#} p $ (fromIntegral.fromEnum) KeepLastHistoryQos
      {#set DDS_DurabilityServiceQosPolicy.history_depth#} p $ fromIntegral n
    pk (MaxSamples ms) =
      {#set DDS_DurabilityServiceQosPolicy.max_samples#} p $ fromIntegral ms
    pk (MaxInstances mi) =
      {#set DDS_DurabilityServiceQosPolicy.max_instances#} p $ fromIntegral mi
    pk (MaxSamplesPerInstance mspi) =
      {#set DDS_DurabilityServiceQosPolicy.max_samples_per_instance#} p $ fromIntegral mspi
    pk _ = return ()
