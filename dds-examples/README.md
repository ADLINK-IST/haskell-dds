# HaskellDDS examples

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
* the “ks|” bit at the very beginning sets a prefix for all the record fields names defined in this block of IDL. The program doesn’t access any fields in the record, but because of the fixed prefix, the names will be “ksSeq”, “ksKeyval”, etc. Alternatively, the prefix can be specified per type using “#pragma TYPE prefix”. If no prefix is specified, it would have defaulted to the type name in lower case — i.e., “keyedseqSeq”, &c., which is not always to be desired.
* “newWaitsetC” is a convenience function, equivalent to calling newWaitset, then newReadCondition, and finally attaching the read condition.

## sub

Sub is a very limited implementation of one subscription mode of the “pubsub” tool in the <https://www.github.com/prismtech/opensplice-tools> repository: when “pub” is run in throughput testing mode (or at least generating PubSub samples of type KeyedSeq while incrementing the sequence number) publishing in partition “test” — i.e., it plays nicely with `pubsub -m0 -w1 test`.

The peculiar thing about “sub” is that uses a hand-written conversion between the Haskell data type and the memory layout required required by OpenSplice. It is also possible to integrate OpenSplice's IDL preprocessor and (e.g.) c2hs for getting offset calculations right, but that is a bit more involved (as it requires multiple preprocessing stages as well as introducing additional compilation and linking stages.

## genread

Genread is an example of a generic reader — it subscribes to DCPSPublication topics, and then automatically creates new readers whenever a new writer for a topic is discovered. It then prints the data in JSON. It shows the use of using the embedded IDL to define a reduced version of a topic type, then use findTopic and read the data, as well as defining a type by hand and making it an instance of the `ToJSON` and `FromJSON` classes.

## cohtest

This is a test of OpenSplice behaviour with coherent sets in a transient topic, but it shows the usage of group coherency.
