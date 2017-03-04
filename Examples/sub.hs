import DDS
import Examples.KeyedSeq

checkseqs :: [(a, KeyedSeq)] -> Integer -> IO Integer
checkseqs [] nseq = return nseq
checkseqs ((_,d):ds) nseq
  | ksSeq d == nseq = checkseqs ds (nseq+1)
  | otherwise = do putStrLn $ "out-of-order " ++ show (ksSeq d) ++ " exp " ++ show nseq ; checkseqs ds (ksSeq d + 1)

main :: IO ()
main = do
  dp <- newParticipant DefaultDomain
  sub <- newSubscriber [Partition ["test"]] dp
  tp <- newTopic [Reliability Reliable, MaxBlockingTime 1.0, Order BySource] "PubSub" dp
  rd <- newReader [MaxSamples 1000, History KeepAll] tp sub
  ws <- newWaitsetC [(newReadCondition [] rd, (1::Int))]
  mainloop ws rd 0
  where
    mainloop ws rd nseq = do
      wait ws
      d <- takeAll rd
      nseq' <- checkseqs d nseq
      mainloop ws rd nseq'
