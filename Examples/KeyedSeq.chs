{- -*- haskell -*- -}

module Examples.KeyedSeq (KeyedSeq(..), nullKeyedSeq, peekKeyedSeq) where

import DDS
import qualified DDS.Raw as R

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Control.Monad

-- Work around __attribute__ specs used on MacOS X for declaring which
-- versions implement what
#define __attribute__(x)
#include "dds_dcps_mangled.h"
#include "testtype.h"

data KeyedSeq = KeyedSeq
                { ksKeyval :: Integer
                , ksSeq :: Integer
                , ksBaggage :: [CUChar]
                } deriving (Show)
{#pointer *KeyedSeq as KeyedSeqPtr -> KeyedSeq#}

nullKeyedSeq :: KeyedSeq
nullKeyedSeq = KeyedSeq { ksKeyval = 0, ksSeq = 0, ksBaggage = [] }

foreign import ccall "KeyedSeqTypeSupport__alloc" c_KeyedSeqTypeSupport__alloc :: IO (R.TypeSupport KeyedSeq)

peekKeyedSeq :: Ptr KeyedSeq -> IO KeyedSeq
peekKeyedSeq p = do
  xkey <- liftM fromIntegral $ {#get KeyedSeq.keyval#} p
  xseq <- liftM fromIntegral $ {#get KeyedSeq.seq#} p
  xtmp <- peek (castPtr $ p `plusPtr` {#offsetof KeyedSeq.baggage#})
  xbs <- peekArray (R.seqLength xtmp) (R.seqBuffer xtmp)
  return (KeyedSeq { ksKeyval = xkey, ksSeq = xseq, ksBaggage = xbs })

pokeKeyedSeq :: Ptr KeyedSeq -> KeyedSeq -> (Ptr KeyedSeq -> IO b) -> IO b
pokeKeyedSeq p ks fun =
  let n = length $ ksBaggage ks
  in do
    {#set KeyedSeq.keyval#} p $ fromIntegral $ ksKeyval ks
    {#set KeyedSeq.seq#} p $ fromIntegral $ ksSeq ks
    allocaBytes (n * sizeOf (undefined :: CUChar)) $ \a -> do
      pokeArray a $ ksBaggage ks
      let tmp = R.Sequence { R.seqLength = n, R.seqMaximum = n, R.seqRelease = True, R.seqBuffer = a }
      poke (castPtr $ p `plusPtr` {#offsetof KeyedSeq.baggage#}) tmp
      fun p

instance R.TopicClass KeyedSeq where
  sizeofSample _ = R.SampleSize {#sizeof KeyedSeq#}
  peekSample _ p = peekKeyedSeq p
  withSample _ s f = allocaBytes {#sizeof KeyedSeq#} $ \ptr -> pokeKeyedSeq ptr s f

instance NewTopicClass KeyedSeq where
  newTypeSupport _ = c_KeyedSeqTypeSupport__alloc
