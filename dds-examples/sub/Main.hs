import DDS
import KeyedSeq
import Data.Word
import Data.Time

checkseqs :: [(a, KeyedSeq)] -> Word32 -> IO Word32
checkseqs [] nseq = return nseq
checkseqs ((_,d):ds) nseq
  | ksSeq d == nseq = checkseqs ds (nseq+1)
  | otherwise = do putStrLn $ "out-of-order " ++ show (ksSeq d) ++ " exp " ++ show nseq ; checkseqs ds (ksSeq d + 1)

maybePrint tprint nprint nseq = do
  now <- getCurrentTime
  if now >= tprint
    then do
      putStrLn $ show now ++ " " ++ show (nseq - nprint)
      return (addUTCTime 1 now, nseq)
    else
      return (tprint, nprint)

main :: IO ()
main = do
  dp <- newParticipant DefaultDomain
  sub <- newSubscriber [Partition ["test"]] dp
  tp <- newTopic [Reliability Reliable, MaxBlockingTime 1.0, Order BySource] "PubSub" dp
  rd <- newReader [MaxSamples 1000, History KeepAll] tp sub
  ws <- newWaitset
  rc <- newReadCondition [] rd
  attach ws rc (1::Int)
  tprint <- getCurrentTime
  mainloop ws rd (addUTCTime 1 tprint, 0) 0
  where
    mainloop ws rd (tprint, nprint) nseq = do
      wait ws
      d <- takeAll rd
      nseq' <- checkseqs d nseq
      (tprint', nprint') <- maybePrint tprint nprint nseq
      mainloop ws rd (tprint', nprint') nseq'
