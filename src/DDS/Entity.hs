module DDS.Entity where

import qualified DDS.Type as U
import qualified DDS.Raw as R

data Participant = Participant R.DomainParticipant
data Topic a = Topic Participant R.Topic U.TopicType String
data Subscriber = Subscriber Participant R.Subscriber
data Publisher = Publisher Participant R.Publisher
data Reader a = Reader Subscriber (Topic a) U.TopicType R.DataReader
data Writer a = Writer Publisher (Topic a) U.TopicType R.DataWriter

instance Show (Topic a) where
  show (Topic _ _ tt nm) = nm ++ ": " ++ show tt

instance Show (Reader a) where
  show (Reader _ tp _ _) = show "Reader of " ++ show tp

instance Show (Writer a) where
  show (Writer _ tp _ _) = show "Writer of " ++ show tp
