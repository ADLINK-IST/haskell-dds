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
  ws <- newWaitset
  rc <- newReadCondition [] rd
  attach ws rc (respond wr rd)
  forever $ wait ws >>= sequence_
