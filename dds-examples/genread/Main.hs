{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, TemplateHaskell, QuasiQuotes, StandaloneDeriving #-}

import GHC.Generics
import Data.Char
import Data.Aeson
import Data.Aeson.Types
import Control.Monad.State
import Control.Monad (forever, sequence_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.UTF8 as B (toString)

import DDS

data St = St { stParticipant :: Participant
             , stSubscriber :: Subscriber
             , stWaitset :: Waitset (StateT St IO ())
             , stReaders :: [String] }

type Action a = SampleInfo -> a -> StateT St IO ()
type Handler a = String -> Reader a -> StateT St IO ()

-- | Takes disposed instances and reads all unread samples, invoking a
--   create, update or delete handler as appropriate
docrud :: TopicClass a => Action a -> Action a -> Action a -> Reader a -> StateT St IO ()
docrud cr upd del rd = do
  (liftIO $ DDS.take [IState Disposed] rd) >>= mapM_ (uncurry del)
  (liftIO $ DDS.read [SState NotRead, IState Alive, IState NoWriters] rd) >>= mapM_ (uncurry crupd)
  where
    crupd si x | viewState si == New = cr si x
               | otherwise = upd si x

-- | Generic handler for create, update and delete by printing a JSON
--   representation of the data, prefixed by "C", "U" or "D" depending
--   on whether it is a create, an update or a delete.
docrudData :: Handler Object
docrudData tpname = docrud (h "C ") (h "U ") (h "D ")
  where
    h op _ obj = liftIO $ putStrLn $ op ++ tpname ++ ": " ++ stringify obj
    stringify = B.toString . encode

-- | Just because I can, special handling of the "d_nameSpaces"
--   topic. The type definition covers a subset of the real topic
--   definition (with the correct field names and enum values, all
--   other fields in the real topic are simply ignored), and we use
--   findTopic to bind to the topic so OSPL never learns of our
--   strange definition.
[topicidl|
  struct Na { long systemId; };
  enum Ds {
    D_STATE_INIT, D_STATE_DISCOVER_FELLOWS_GROUPS, D_STATE_DISCOVER_LOCAL_GROUPS, 
    D_STATE_DISCOVER_PERSISTENT_SOURCE, D_STATE_INJECT_PERSISTENT, D_STATE_FETCH_INITIAL,
    D_STATE_COMPLETE, D_STATE_FETCH, D_STATE_ALIGN, D_STATE_FETCH_ALIGN, D_STATE_TERMINATING,
    D_STATE_TERMINATED };
  struct Dm { Na senderAddress; Ds senderState; };
  struct Ns { Dm parentMsg; string name; Na master; bool isComplete; bool masterConfirmed; };
  #pragma keylist Ns parentMsg.senderAddress.systemId |]

doNameSpaces :: Handler Ns
doNameSpaces tpname = docrud h h h
  where
    h _ obj = let src = (naSystemId . dmSenderAddress . nsParentMsg) obj
                  mas = (naSystemId . nsMaster) obj
                  ns = nsName obj
                  conf = if nsMasterConfirmed obj then " confirmed" else " unconfirmed"
              in liftIO $ putStrLn $ tpname ++ ": " ++ show src ++ " " ++ ns ++ " master " ++ show mas ++ conf

-- | Add a reader for a topic (given by name) with a specified
--   handler, reader QoS is the topic QoS with the history forced to
--   keep-last 1
addRd' :: TopicClass a => Handler a -> String -> StateT St IO ()
addRd' h tpname = do
  liftIO $ putStrLn $ "adding reader for " ++ tpname
  St{..} <- get
  liftIO $ do
    tp <- findTopic (-1) tpname stParticipant
    rd <- newReader [History (KeepLast 1)] tp stSubscriber
    attach stWaitset rd $ h tpname rd

-- | Add a reader for the topic (given by name), excepting a hardcoded list of "builtins"
addRd :: String -> StateT St IO ()
addRd tpname
  | tpname `elem` builtins = return ()
  | tpname == "d_nameSpaces" = addRd' doNameSpaces tpname
  | otherwise = addRd' docrudData tpname
  where
    builtins = [ "DCPSTopic", "DCPSSubscription", "DCPSPublication", "DCPSParticipant"
               , "CMParticipant", "CMDataReader", "CMDataWriter", "CMSubscriber"
               , "CMPublisher", "DCPSType", "DCPSCandMCommand", "DCPSDelivery"
               , "DCPSHeartbeat", "q_bubble", "d_status" ]

-- | Minimalistic data definition for reading DCPSPublication
jsonOpts = defaultOptions { fieldLabelModifier = lowerFirst . drop 2 }
  where
    lowerFirst [] = []
    lowerFirst (x:xs) = toLower x : xs
data Dp = Dp { dpKey :: (Int, Int, Int), dpTopic_name :: String } deriving (Show, Generic)
instance ToJSON Dp where toJSON = genericToJSON jsonOpts
instance FromJSON Dp where parseJSON = genericParseJSON jsonOpts

-- | Create handler for DCPSPublication
crWr _ obj = do
  let tp = dpTopic_name obj
  liftIO $ putStrLn $ "crWr " ++ show (dpKey obj) ++ ": " ++ tp
  rs <- gets stReaders
  unless (tp `elem` rs) $ do
    st@St{..} <- get
    put $ st { stReaders = tp:stReaders }
    addRd tp

-- | Delete handler for DCPSPublication
delWr _ obj = liftIO $ putStrLn $ "delWr: " ++ show (dpKey obj)

newSt :: IO St
newSt = do
  stParticipant <- newParticipant DefaultDomain
  stSubscriber <- newSubscriber [Partition ["*"]] stParticipant
  stWaitset <- newWaitset
  let stReaders = []
  return St{..}

main :: IO ()
main = do
  st@St{..} <- newSt
  pubrd <- getBuiltinReader "DCPSPublication" stParticipant :: IO (Reader Dp)
  let handlePub = docrud crWr (\_ _ -> return ()) delWr
  attach stWaitset pubrd $ handlePub pubrd
  let monitor = forever ((liftIO $ wait stWaitset) >>= sequence_)
  evalStateT monitor st
