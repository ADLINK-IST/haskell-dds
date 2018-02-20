{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}
module DDS.Core (
            DomainId(..), getSystemId, nodeBuiltinPartition,
            R.SampleInfo(..), R.SState(..), R.VState(..), R.IState(..),
            R.TopicClass, NewTopicClass, newTypeSupport,
            R.Retcode(),
            Participant,
            Topic, TopicMD(..),
            Subscriber, Reader,
            newParticipant, newSubscriber, getBuiltinSubscriber,
            Publisher, Writer,
            newPublisher,
            Waitset, Condition, GuardCondition, ReadCondition, StatusCondition,
            ReadConditionKind(..), ReadConditionMask,
            newWaitset, newWaitsetC, newGuardCondition, newReadCondition,
            triggerGuardCondition,
            wait, timedwait, attach, detach,
            DDS.Core.delete, DDS.Core.enable,
            R.Status(..), getStatusCondition, getStatusChanges, setEnabledStatuses,
            write, writeTs, writeDispose, writeDisposeTs, dispose, disposeTs, unregister, unregisterTs,
            DDS.Core.read, DDS.Core.take, readN, takeN, readAll, takeAll,
            newReader, lookupReader, newWriter,
            newTopic, newGenericTopic, findTopic, topicType,
            beginCoherent, endCoherent, withCoherent,
            getSubscriber, getPublisher, getTopic, topicName, getBuiltinReader,
            waitForHistoricalData,
            module DDS.Qos
           ) where

import Control.Concurrent.MVar
import Control.Exception (bracket, bracket_)
import Control.Monad (void)
import Foreign.Ptr
import Foreign.Storable

import Data.Data
import Data.Maybe
import Data.Typeable
import Data.List
import Numeric (showHex)
import qualified Data.HashMap.Strict as M

import DDS.Qos
import DDS.Entity
import DDS.TopicXML
import qualified DDS.Type as U
import qualified DDS.Raw as R

-- | Class of generic operations on entities and (in this binding)
--   entity-like objects such as waitsets and read conditions
class Entity a where
  -- | This function deletes the entity and any contained entities
  delete :: a -> IO ()
  -- | This functions enables a disabled entity (required for group-coherent subscribers)
  enable :: a -> IO ()

-- | Class of operations on entities that have a status (i.e., any
--   DCPS entity, but not a Waitset)
class StatusEntity a where
  -- | This function returns the status condition corresponding to the entity
  getStatusCondition :: a -> IO StatusCondition
  -- | This function returns the current set changed statuses
  getStatusChanges :: a -> IO [R.Status]

-- | Coherent operations, mapping to beginCoherent, endCoherent on a
--   Publisher and to beginAccess, endAccess on a Subscriber; and for
--   good measure also to the above when invoked on a Writer or a
--   Reader.
class CoherentEntity a where
  -- | Enters a coherent region (whether for writing or for reading)
  beginCoherent :: a -> IO ()
  -- | Leaves a coherent region (whether for writing or for reading)
  endCoherent :: a -> IO ()

-- | Class of operations on entities that have an associated topic
--   (i.e., Readers and Writers)
class TopicEntity a b where
  -- | returns the Topic
  getTopic :: a b -> Topic b

-- | An entity that can be "converted" into a subscriber, for a
--   Participant implemented as a new default-QoS subscriber, for a
--   Subscriber implemented as the subscriber itelf.  For very simple
--   applications, this eliminates having to create a subscriber.
class GetSubscriberIO a where
  getSubscriberIO :: a -> IO Subscriber
instance GetSubscriberIO Participant where
  getSubscriberIO = newSubscriber []
instance GetSubscriberIO Subscriber where
  getSubscriberIO = return . id
instance GetSubscriberIO (Reader a) where
  getSubscriberIO = return . getSubscriber

-- | An entity that can be "converted" into a publisher, for a
--   Participant implemented as a new default-QoS publisher, for a
--   Publisher implemented as the publisher itelf.  For very simple
--   applications, this eliminates having to create a publisher.
class GetPublisherIO a where
  getPublisherIO :: a -> IO Publisher
instance GetPublisherIO Participant where
  getPublisherIO = newPublisher []
instance GetPublisherIO Publisher where
  getPublisherIO = return . id
instance GetPublisherIO (Writer a) where
  getPublisherIO = return . getPublisher

-- | A convenience function that performs the passed function
--   bracketed by beginCoherent/endCoherent on the entity
withCoherent :: CoherentEntity a => a -> IO b -> IO b
withCoherent e f = bracket_ (beginCoherent e) (endCoherent e) f

-- | Class of Topics for which we can call newTopic, which requires
--   being able to create a type support, unlike findTopic which
--   relies on the generic type support
class R.TopicClass a => NewTopicClass a where
  newTypeSupport :: a -> IO (R.TypeSupport a)

-- | Domain id encoding for specifying the domain id when creating a
--   new participant
data DomainId = DefaultDomain | DomainId Int deriving (Show, Eq, Ord)

-- | Create a new particpant
newParticipant :: DomainId -> IO Participant
newParticipant mdid = do
  mdp <- R.createParticipant did
  case mdp of
    (Left msg) -> fail msg
    (Right dp) -> return $ Participant dp
  where
    did = case mdid of
      DefaultDomain -> R.defaultDomainId
      DomainId id -> fromIntegral id

-- | The systemId for the federation containing the participant
getSystemId :: Participant -> IO Integer
getSystemId (Participant idp) = R.getSystemId idp

-- | The name of the built-in partition specific to the federation
--   containing the participant
nodeBuiltinPartition :: Integer -> String
nodeBuiltinPartition sysid = "__NODE" ++ hpad ++ " BUILT-IN PARTITION__"
  where
    h = showHex sysid ""
    hpad = replicate (8 - length h) '0' ++ h

-- | Create a new subscriber with the given QoS in the particpant
newSubscriber :: Qos -> Participant -> IO Subscriber
newSubscriber qos dp@(Participant idp) = do
  msub <- R.createSubscriber idp qos
  case msub of
    Nothing -> fail "cannot create subscriber"
    (Just isub) -> return $ Subscriber dp isub

-- | Get the built-in subscriber of the particpant
getBuiltinSubscriber :: Participant -> IO Subscriber
getBuiltinSubscriber dp@(Participant idp) = do
  isub <- R.getBuiltinSubscriber idp
  return $ Subscriber dp isub

-- | Create a new publisher with the given QoS in the particpant
newPublisher :: Qos -> Participant -> IO Publisher
newPublisher qos dp@(Participant idp) = do
  mpub <- R.createPublisher idp qos
  case mpub of
    Nothing -> fail "cannot create publisher"
    (Just ipub) -> return $ Publisher dp ipub

-- passing TypeSupport only to disambiguate the type of newTypeSupport in newTopic ...
newTopicImpl :: R.TopicClass a => Participant -> Qos -> String -> String -> R.TypeSupport a -> IO (Topic a)
newTopicImpl dp@(Participant idp) qos topicName typeName ts = do
  mtp <- R.createTopic idp topicName typeName qos
  case mtp of
    Nothing -> fail $ "cannot create topic " ++ topicName
    (Just itp) -> do
      md <- R.getTopicMetaDescription itp
      tn <- R.getTopicTypeName itp
      let mtt = getTopicType md tn
      case mtt of
        Nothing -> fail $ "cannot parse type " ++ topicName
        (Just tt) -> return $ Topic dp itp tt topicName

-- | Create a new topic with the given QoS and name in the
--   participant.
newTopic :: NewTopicClass a => Qos -> String -> Participant -> IO (Topic a)
newTopic qos name dp@(Participant idp) = do
  ts <- newTypeSupport undefined
  typeName <- R.getTypeName ts
  regres <- R.registerType ts idp
  res <- case regres of
    R.RetcodeOk -> newTopicImpl dp qos name typeName ts
    _ -> fail $ "newTopic " ++ name ++ " failed to register type " ++ show typeName
  R.freeTypeSupport ts
  return res

newGenericTopic :: R.TopicClass a => Qos -> TopicMD -> String -> Participant -> IO (Topic a)
newGenericTopic qos tmd name dp@(Participant idp) = do
  let tn = topicMDTypename tmd
      kl = topicMDKeylist tmd
      md = topicMDMetadata tmd
  rc <- R.registerGenericTypeSupport idp tn kl md
  res <- case rc of
    R.RetcodeOk -> newTopicImpl dp qos name tn undefined
    _ -> fail ("newGenericTopic " ++ name ++ "failed to register generic type support for " ++ tn)
  return res

isBuiltinTopic :: String -> Bool
isBuiltinTopic name = name `elem` ["DCPSParticipant", "DCPSTopic", "DCPSPublication", "DCPSSubscription"]

-- | Lookup a topic by name (using the DCPS find_topic operation on
--   the participant) with a timeout in seconds.  This always
--   registers a generic type support in the OSPL C API.  There is no
--   verification that the Haskell type matches the DCPS type (this
--   can be used to one's advantage as well, especially when reading
--   ...)
findTopic :: R.TopicClass a => Double -> String -> Participant -> IO (Topic a)
findTopic timeout name dp@(Participant idp)
  | isBuiltinTopic name = do
      builtinSub <- R.getBuiltinSubscriber idp
      builtinReader <- R.lookupDataReader builtinSub name
      Just itp <- R.findTopic idp name timeout
      md <- R.getTopicMetaDescription itp
      tn <- R.getTopicTypeName itp
      let Just tt = getTopicType md tn
      return $ Topic dp itp tt name
  | otherwise = do
      mtp <- R.findTopic idp name timeout
      case mtp of
        Nothing -> fail ("findTopic " ++ name ++ " failed")
        Just itp -> do
          md <- R.getTopicMetaDescription itp
          kl <- R.getTopicKeyList itp
          tn <- R.getTopicTypeName itp
          rc <- R.registerGenericTypeSupport idp tn kl md
          case rc of
            R.RetcodeOk -> case getTopicType md tn of
              Nothing -> fail ("findTopic " ++ name ++ " cannot parse type " ++ tn)
              (Just tt) -> return $ Topic dp itp tt name
            _ -> fail ("findTopic " ++ name ++ "failed to register generic type support for " ++ tn)

-- | Get the built-in reader for one fo the built-in topics
--   (DCPSParticipant, DCPSTopic, DCPSPublication, DCPSSubscription),
--   relying on findTopic to lift the topic into Haskell, with the risk
--   of having a mismatching type.
getBuiltinReader :: R.TopicClass a => String -> Participant -> IO (Reader a)
getBuiltinReader name dp@(Participant idp)
  | not (isBuiltinTopic name) = error $ name ++ " is not a supported built-in topic"
  | otherwise = do
      builtinSub <- R.getBuiltinSubscriber idp
      Just ird <- R.lookupDataReader builtinSub name
      tp <- findTopic 0.0 name dp
      return $ Reader (Subscriber dp builtinSub) tp (topicType tp) ird

-- | This function yields the type definition for the topic
topicType :: Topic a -> U.TopicType
topicType (Topic _ _ tt _) = tt

-- | This function yields the name of the topic
topicName :: Topic a -> String
topicName (Topic _ _ _ nm) = nm

-- | Writes a value
write :: R.TopicClass a => Writer a -> a -> IO R.Retcode
write (Writer _ _ tt iwr) = R.write tt iwr

-- | Writes a value with a specific timestamp
writeTs :: R.TopicClass a => Writer a -> Integer -> a -> IO R.Retcode
writeTs (Writer _ _ tt iwr) = R.writeT tt iwr

-- | Write-disposes a value
writeDispose :: R.TopicClass a => Writer a -> a -> IO R.Retcode
writeDispose (Writer _ _ tt iwr) = R.writeDispose tt iwr

-- | Write-disposes a value with a specific timestamp
writeDisposeTs :: R.TopicClass a => Writer a -> Integer -> a -> IO R.Retcode
writeDisposeTs (Writer _ _ tt iwr) = R.writeDisposeT tt iwr

-- | Disposes the instance indicated by the value
dispose :: R.TopicClass a => Writer a -> a -> IO R.Retcode
dispose (Writer _ _ tt iwr) = R.dispose tt iwr

-- | Disposes the instance indicated by the value with a specific timestamp
disposeTs :: R.TopicClass a => Writer a -> Integer -> a -> IO R.Retcode
disposeTs (Writer _ _ tt iwr) = R.disposeT tt iwr

-- | Unregisters the instance indicated by the value
unregister :: R.TopicClass a => Writer a -> a -> IO R.Retcode
unregister (Writer _ _ tt iwr) = R.unregister tt iwr

-- | Unregisters the instance indicated by the value with a specific timestamp
unregisterTs :: R.TopicClass a => Writer a -> Integer -> a -> IO R.Retcode
unregisterTs (Writer _ _ tt iwr) = R.unregisterT tt iwr

-- | Read conditions are specified as a list of sample, view and
--   instance states of interest.  An empty list is equivalent to
--   specifying all states explicitly.
data ReadConditionKind = SState R.SState | VState R.VState | IState R.IState deriving (Eq, Show)
type ReadConditionMask = [ReadConditionKind]

internalReadcondFromMask :: ReadConditionMask -> (Int, Int, Int)
internalReadcondFromMask mask = (xnull s, xnull v, xnull i)
  where
    (s,v,i) = foldl' tr (0,0,0) mask
    tr (s,v,i) (SState x) = (s + fromEnum x, v, i)
    tr (s,v,i) (VState x) = (s, v + fromEnum x, i)
    tr (s,v,i) (IState x) = (s, v, i + fromEnum x)
    xnull s = if s == 0 then 65535 else s

-- | Reads at most N samples matching the conditions (N = 0: unlimited)
readN :: R.TopicClass a => Int -> ReadConditionMask -> Reader a -> IO [(R.SampleInfo, a)]
readN maxn mask (Reader _ _ tt ird) = R.read tt ird maxn sst vst ist
  where (sst, vst, ist) = internalReadcondFromMask mask

-- | Takes at most N samples matching the conditions (N = 0: unlimited)
takeN :: R.TopicClass a => Int -> ReadConditionMask -> Reader a -> IO [(R.SampleInfo, a)]
takeN maxn mask (Reader _ _ tt ird) = R.take tt ird maxn sst vst ist
  where (sst, vst, ist) = internalReadcondFromMask mask

-- | Reads all samples matching the conditions
read :: R.TopicClass a => ReadConditionMask -> Reader a -> IO [(R.SampleInfo, a)]
read = DDS.Core.readN 0

-- | Takes all samples matching the conditions
take :: R.TopicClass a => ReadConditionMask -> Reader a -> IO [(R.SampleInfo, a)]
take = DDS.Core.takeN 0

-- | Reads all samples
readAll :: R.TopicClass a => Reader a -> IO [(R.SampleInfo, a)]
readAll = DDS.Core.read []

-- | Takes all samples
takeAll :: R.TopicClass a => Reader a -> IO [(R.SampleInfo, a)]
takeAll = DDS.Core.take []

instance TopicEntity Reader a where
  getTopic (Reader _ tp _ _) = tp

instance TopicEntity Writer a where
  getTopic (Writer _ tp _ _) = tp

-- | Creates a new reader for the given topic and with a QoS
--   override. The QoS used is the default reader QoS, modified by the
--   topic QoS, modified by the QoS given here.  When the parent is a
--   participant, this creates a default subscriber just for this one
--   reader.
newReader :: (R.TopicClass a, GetSubscriberIO p) => Qos -> Topic a -> p -> IO (Reader a)
newReader qos tp@(Topic _ itp tt _) parent = do
  sub@(Subscriber _ isub) <- getSubscriberIO parent
  mrd <- R.createDataReader isub itp qos
  case mrd of
    Nothing -> fail "cannot create reader"
    (Just ird) -> return $ Reader sub tp tt ird

-- | Returns the subscriber containing the reader
getSubscriber :: Reader a -> Subscriber
getSubscriber (Reader sub _ _ _) = sub

-- | Wait for historical data with a timeout (< 0 is infinite), return
--   True on success and False on timeout
waitForHistoricalData :: Double -> Reader a -> IO Bool
waitForHistoricalData timeout (Reader _ _ _ ird)
  | timeout >= 0 && timeout /= infinity = wait' tsec tnsec ird
  | otherwise = wait' 2147483647 2147483647 ird
  where
    infinity = Prelude.read "Infinity" :: Double
    i = floor timeout :: Integer
    tsec = fromIntegral i
    tnsec = floor ((timeout - fromIntegral i) * 1e9)
    wait' tsec tnsec rd = do
      rc <- R.waitForHistoricalData rd tsec tnsec
      case rc of
        R.RetcodeOk -> return True
        R.RetcodeTimeout -> return False
        _ -> fail $ "error in waitForHistoricalData: " ++ show rc

-- | Creates a new writer for the given topic and with a QoS
--   override. The QoS used is the default writer QoS, modified by the
--   topic QoS, modified by the QoS given here.  When the parent is a
--   participant, this creates a default publisher just for this one
--   writer.
newWriter :: (R.TopicClass a, GetPublisherIO p) => Qos -> Topic a -> p -> IO (Writer a)
newWriter qos tp@(Topic _ itp tt _) parent = do
  pub@(Publisher _ ipub) <- getPublisherIO parent
  mwr <- R.createDataWriter ipub itp qos
  case mwr of
    Nothing -> fail "cannot create writer"
    (Just iwr) -> return $ Writer pub tp tt iwr

-- | Returns the publisher containing the writer
getPublisher :: Writer a -> Publisher
getPublisher (Writer pub _ _ _) = pub

-- | This function looks up a reader in a subscriber for a given
--   topic.  Unlike the DCPS function, this one requires a proper
--   topic rather than a topic name.  While this doesn't change much,
--   it does mean the type hole is limited to findTopic.
lookupReader :: R.TopicClass a => Topic a -> Subscriber -> IO (Maybe (Reader a))
lookupReader tp@(Topic _ itp tt _) sub@(Subscriber _ isub) = do
  topicName <- R.getTopicName itp
  mrd <- R.lookupDataReader isub topicName
  case mrd of
    Nothing -> return Nothing
    Just ird -> return $ Just $ Reader sub tp tt ird

okOrFail :: String -> R.Retcode -> IO ()
okOrFail _ R.RetcodeOk = return ()
okOrFail op rc = fail $ "error in " ++ op ++ ": " ++ show rc

instance CoherentEntity Publisher where
  beginCoherent (Publisher _ ipub) = R.beginCoherent ipub >>= okOrFail "beginCoherent"
  endCoherent (Publisher _ ipub) = R.endCoherent ipub >>= okOrFail "endCoherent"
instance CoherentEntity (Writer a) where
  beginCoherent = beginCoherent . getPublisher
  endCoherent = endCoherent . getPublisher

instance CoherentEntity Subscriber where
  beginCoherent (Subscriber _ isub) = R.beginAccess isub >>= okOrFail "beginAccess"
  endCoherent (Subscriber _ isub) = R.endAccess isub >>= okOrFail "endAccess"
instance CoherentEntity (Reader a) where
  beginCoherent = beginCoherent . getSubscriber
  endCoherent = endCoherent . getSubscriber

instance Entity Participant where
  delete (Participant dp) = R.deleteParticipant dp >>= okOrFail "delete participant"
  enable (Participant dp) = R.enableParticipant dp >>= okOrFail "enable participant"
instance Entity (Topic a) where
  delete (Topic (Participant idp) tp _ _) = R.deleteTopic idp tp >>= okOrFail "delete topic"
  enable (Topic _ tp _ _) = R.enableTopic tp >>= okOrFail "enable topic"
instance Entity Subscriber where
  delete (Subscriber (Participant idp) sub) = R.deleteSubscriber idp sub >>= okOrFail "delete subscriber"
  enable (Subscriber _ sub) = R.enableSubscriber sub >>= okOrFail "enable subscriber"
instance Entity (Reader a) where
  delete (Reader (Subscriber _ isub) _ _ rd) = R.deleteDataReader isub rd >>= okOrFail "delete reader"
  enable (Reader _ _ _ rd) = R.enableDataReader rd >>= okOrFail "enable reader"
instance Entity Publisher where
  delete (Publisher (Participant idp) pub) = R.deletePublisher idp pub >>= okOrFail "delete publisher"
  enable (Publisher _ pub) = R.enablePublisher pub >>= okOrFail "enable publisher"
instance Entity (Writer a) where
  delete (Writer (Publisher _ ipub) _ _ wr) = R.deleteDataWriter ipub wr >>= okOrFail "delete writer"
  enable (Writer _ _ _ wr) = R.enableDataWriter wr >>= okOrFail "enable writer"

data GuardCondition = GuardCondition R.GuardCondition
data ReadCondition = ReadCondition R.DataReader R.ReadCondition
data StatusCondition = StatusCondition R.StatusCondition

class Condition c where
  -- | Attach a condition and an arbitrary associated value to a waitset
  attach :: Waitset a -> c -> a -> IO ()
  -- | Detach a condition from a waitset
  detach :: Waitset a -> c -> IO ()

-- | This function creates a new guard condition
newGuardCondition :: IO GuardCondition
newGuardCondition = do
  cond <- R.createGuardCondition
  case cond of
    Nothing -> fail "cannot create guard condition"
    (Just c) -> return $ GuardCondition c

-- | This function triggers a guard condition
triggerGuardCondition :: GuardCondition -> IO ()
triggerGuardCondition (GuardCondition c) = void $ R.triggerGuardCondition c

-- | This function creates a new read condition on a reader, with the
--   interest specified the same way as for read and take
newReadCondition :: ReadConditionMask -> Reader a -> IO ReadCondition
newReadCondition mask (Reader _ _ _ ird) = do
  let (sst, vst, ist) = internalReadcondFromMask mask
  cond <- R.createReadCondition ird sst vst ist
  case cond of
    Nothing -> fail "cannot create read condition"
    (Just c) -> return $ ReadCondition ird c

instance Entity GuardCondition where
  delete (GuardCondition cond) = R.deleteGuardCondition cond >> return ()
  enable (GuardCondition _) = return ()
instance Entity ReadCondition where
  delete (ReadCondition ird cond) = R.deleteReadCondition ird cond >> return ()
  enable (ReadCondition _ _) = return ()
instance Entity StatusCondition where
  delete (StatusCondition _) = return () -- no delete needed
  enable (StatusCondition _) = return ()

instance StatusEntity Participant where
  getStatusCondition (Participant dp) = R.participantStatusCondition dp >>= return . StatusCondition
  getStatusChanges (Participant dp) = R.participantStatusChanges dp
instance StatusEntity Subscriber where
  getStatusCondition (Subscriber _ sub) = R.subscriberStatusCondition sub >>= return . StatusCondition
  getStatusChanges (Subscriber _ sub) = R.subscriberStatusChanges sub
instance StatusEntity (Reader a) where
  getStatusCondition (Reader _ _ _ rd) = R.dataReaderStatusCondition rd >>= return . StatusCondition
  getStatusChanges (Reader _ _ _ rd) = R.dataReaderStatusChanges rd
instance StatusEntity Publisher where
  getStatusCondition (Publisher _ pub) = R.publisherStatusCondition pub >>= return . StatusCondition
  getStatusChanges (Publisher _ pub) = R.publisherStatusChanges pub
instance StatusEntity (Writer a) where
  getStatusCondition (Writer _ _ _ wr) = R.dataWriterStatusCondition wr >>= return . StatusCondition
  getStatusChanges (Writer _ _ _ wr) = R.dataWriterStatusChanges wr

-- | This function sets the enabled statuses for a status condition
setEnabledStatuses :: [R.Status] -> StatusCondition -> IO ()
setEnabledStatuses st (StatusCondition isc) = R.setEnabledStatuses st isc >> return ()

instance Condition GuardCondition where
  attach ws (GuardCondition cond) tag = R.withGuardCondition cond $ \c -> attachCond ws cond (castPtr c) tag
  detach ws (GuardCondition cond) = R.withGuardCondition cond $ \c -> detachCond ws cond (castPtr c)

instance Condition ReadCondition where
  attach ws (ReadCondition _ cond) tag = R.withReadCondition cond $ \c -> attachCond ws cond (castPtr c) tag
  detach ws (ReadCondition _ cond) = R.withReadCondition cond $ \c -> detachCond ws cond (castPtr c)

instance Condition StatusCondition where
  attach ws (StatusCondition cond) tag = R.withStatusCondition cond $ \c -> attachCond ws cond (castPtr c) tag
  detach ws (StatusCondition cond) = R.withStatusCondition cond $ \c -> detachCond ws cond (castPtr c)

-- | The Haskell binding allows attaching a reader to a waitset, by
--   implementing it as a attaching the readers status condition with
--   the DataAvailable status enabled
instance Condition (Reader a) where
  attach ws rd tag = do
    sc <- getStatusCondition rd
    setEnabledStatuses [R.DataAvailable] sc
    attach ws sc tag
  detach ws rd = do
    sc <- getStatusCondition rd
    detach ws sc

data Waitset a = Waitset R.Waitset (MVar (M.HashMap (Ptr R.Condition) a))

instance Entity (Waitset a) where
  delete (Waitset ws m) = do R.deleteWaitset ws ; _ <- swapMVar m (M.empty) ; return ()
  enable (Waitset _ _) = return ()

-- | This function creates a new empty waitset
newWaitset :: IO (Waitset a)
newWaitset = do
  ws <- R.createWaitset
  case ws of
    Nothing -> fail "cannot create waitset"
    (Just w) -> do mv <- newMVar M.empty ; return $ Waitset w mv

-- | This function creates a new waitset and then attaches the result
--   of the IO actions in the list, so that one can write:
--
--     newWaitsetC [(newReadCondition [IState Disposed] rd, ...), (newReadCondition [SState NotRead] rd, ...)]
--
--   to combine the creation of the waitset, the creation of a number
--   of read conditions and attaching these read conditions to the new
--   waitset.
newWaitsetC :: forall a c. Condition c => [(IO c, a)] -> IO (Waitset a)
newWaitsetC rcs = do
  ws <- newWaitset
  mapM_ (\(c,x) -> c >>= (flip $ attach ws) x) rcs
  return ws

-- | This function waits on a waitset with a time out (< 0: infinite),
--   returning a list of the values associated with the triggered
--   conditions
timedwait :: Double -> Waitset a -> IO [a]
timedwait timeout ws
  | timeout >= 0 && timeout /= infinity = wait' tsec tnsec ws
  | otherwise = wait' 2147483647 2147483647 ws
  where
    infinity = Prelude.read "Infinity" :: Double
    i = floor timeout :: Integer
    tsec = fromIntegral i
    tnsec = floor ((timeout - fromIntegral i) * 1e9)
    wait' :: Integer -> Integer -> Waitset a -> IO [a]
    wait' tsec tnsec (Waitset iws m) =
      bracket (takeMVar m) (putMVar m) $ \bs -> do
        rawres <- R.wait iws tsec tnsec
        case rawres of
          (Left rc) -> fail $ "wait failed " ++ show rc
          (Right cs) -> return $ map (bs M.!) cs

-- | Like timedwait, but with an infinite timeout
wait :: Waitset a -> IO [a]
wait = timedwait (-1)

attachCond :: R.ConditionClass c => Waitset a -> c -> Ptr R.Condition -> a -> IO ()
attachCond (Waitset iws m) cond cptr tag =
  modifyMVar_ m $ \bs -> do
    if cptr `M.member` bs then do
      fail "attach: condition already in waitset"
      return bs
    else do
      rc <- R.attachCondition iws cond
      if rc /= R.RetcodeOk then do
        fail $ "attach: failed " ++ show rc
        return bs
      else do
        return $ M.insert cptr tag bs

detachCond :: R.ConditionClass c => Waitset a -> c -> Ptr R.Condition -> IO ()
detachCond (Waitset iws m) cond cptr =
  modifyMVar_ m $ \bs -> do
    if not $ cptr `M.member` bs then do
      fail "attach: condition not in waitset"
      return bs
    else do
      rc <- R.detachCondition iws cond
      if rc /= R.RetcodeOk then do
        fail $ "detach: failed " ++ show rc
        return bs
      else do
        return $ M.delete cptr bs
