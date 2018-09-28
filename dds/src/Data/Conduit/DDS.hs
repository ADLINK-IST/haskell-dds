{-# LANGUAGE RankNTypes #-}
module Data.Conduit.DDS (sinkDDS, sourceDDS, sourceDDS') where

import Data.Conduit
import qualified Data.Conduit.List as CL

import DDS
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

sourceDDS :: (MonadResource m, TopicClass a) => Reader a -> (forall i. ConduitT i a m ())
sourceDDS rd =
  bracketP (newReadCondition [] rd) delete $ \rcond ->
    bracketP (newWaitset) delete $ \ws -> do
      liftIO $ attach ws rcond rd
      loop ws
  where
    loop ws = do
      liftIO $ wait ws
      xs <- liftIO $ takeN 1 [] rd
      unless (null xs) $ yield (snd $ head xs)
      loop ws

sourceDDS' :: (MonadResource m, TopicClass a) => Reader a -> (forall i. ConduitT i (SampleInfo, a) m ())
sourceDDS' rd =
  bracketP (newReadCondition [] rd) delete $ \rcond ->
    bracketP (newWaitset) delete $ \ws -> do
      liftIO $ attach ws rcond rd
      loop ws
  where
    loop ws = do
      liftIO $ wait ws
      xs <- liftIO $ takeN 1 [] rd
      unless (null xs) $ yield (head xs)
      loop ws

sinkDDS :: (MonadIO m, TopicClass a) => Writer a -> (forall o. ConduitT a o m ())
sinkDDS wr = CL.mapM_ (liftIO . void . write wr)
