{-# LANGUAGE DeriveGeneric, TemplateHaskell, QuasiQuotes, StandaloneDeriving #-}
import DDS
import Data.Maybe
import Control.Monad
import Control.Concurrent
import System.Environment
import System.Exit

[topicidl|
  struct T { long keyval; };
  #pragma keylist T keyval |]
deriving instance Show T

psq = [AccessScope ByGroup, CoherentAccess True]
tq = [Reliability Reliable, Order BySource, Durability Transient]

pub dp tAZ = do
  pub <- newPublisher psq dp
  (wA:wBZ) <- mapM (\tp -> newWriter [AutoDispose False] tp pub) tAZ
  threadDelay 10000000 -- give durability some time to initialise
  withCoherent pub $ write wA (T 1)
  withCoherent pub $ mapM_ (\wr -> write wr (T 1)) wBZ
  putStrLn "sleeping"
  forever $ threadDelay 1000000

sub dp tAZ = do
  sub <- newSubscriber psq dp
  rAZ <- mapM (\tp -> newReader [] tp sub) tAZ
  enable sub
  let act = do
        xs <- withCoherent sub $ mapM takeAll rAZ
        when ((length . concat) xs > 0) $
          void $ mapM (\(nm,x) -> putStrLn $ show nm ++ ":" ++ show x) $ zip ['A' .. 'Z'] xs
  ws <- newWaitset
  mapM_ (\rd -> attach ws rd act) rAZ
  forever (wait ws >>= sequence_)

main = do
  let ops = [("pub", pub), ("sub", sub)]
  args <- getArgs
  when (length args /= 1) $ do putStrLn "usage: coh-test-durability {pub|sub}"; exitFailure
  case lookup (args!!0) ops of
    Nothing -> putStrLn $ "invalid argument: " ++ args!!0
    Just f  -> do
      dp <- newParticipant DefaultDomain
      tAZ <- mapM (\n -> newTopic tq n dp) (map (:[]) ['A' .. 'Z'])
      f dp tAZ
      delete dp
