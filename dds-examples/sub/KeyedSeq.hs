module KeyedSeq (KeyedSeq(..)) where

import DDS
import qualified DDS.Raw as R

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Data.Word

data KeyedSeq = KeyedSeq
                { ksSeq :: Word32
                , ksKeyval :: Word32
                , ksBaggage :: [Word8]
                } deriving (Show)

-- Obviously, using c2hs would make life easier for the calculation of
-- offsets and sample sizes.  That ease comes with a downside: getting
-- the build process correct.  For a simple type like this, it is
-- easier to do it by hand, it still shows the principle of providing
-- everything yourself, but saves me from figuring out how to get
-- Cabal to use idlpp, compile & link C code into a library, run c2hs
-- after idlpp, and link everything together.

peekKeyedSeq :: Ptr KeyedSeq -> IO KeyedSeq
peekKeyedSeq p = do
  xseq <- peek (castPtr p)
  xkey <- peek (castPtr $ p `plusPtr` 4)
  xtmp <- peek (castPtr $ p `plusPtr` 8)
  xbs <- peekArray (R.seqLength xtmp) (R.seqBuffer xtmp)
  return (KeyedSeq { ksKeyval = xkey, ksSeq = xseq, ksBaggage = xbs })

pokeKeyedSeq :: Ptr KeyedSeq -> KeyedSeq -> (Ptr KeyedSeq -> IO b) -> IO b
pokeKeyedSeq p ks fun =
  let n = length $ ksBaggage ks
  in do
    poke (castPtr p) $ ksSeq ks
    poke (castPtr $ p `plusPtr` 4) $ ksKeyval ks
    allocaBytes n $ \a -> do
      pokeArray a $ ksBaggage ks
      let tmp = R.Sequence { R.seqLength = n, R.seqMaximum = n, R.seqRelease = True, R.seqBuffer = a }
      poke (castPtr $ p `plusPtr` 8) tmp
      fun p

instance TopicClass KeyedSeq where
  sizeofSample _ = R.SampleSize (8 + sizeOf (undefined :: R.Sequence Word8))
  peekSample _ p = peekKeyedSeq p
  withSample _ s f = allocaBytes (8 + sizeOf (undefined :: R.Sequence Word8)) $ \ptr -> pokeKeyedSeq ptr s f

-- meta descriptor extracted from idlpp output (idlpp -l c -S x.idl, where x.idl is:
--
-- struct KeyedSeq
-- {
--   unsigned long seq;
--   long keyval;
--   sequence<octet> baggage;
-- };
-- #pragma keylist KeyedSeq keyval
--
-- then look in xSplDcps.c)
metaData = "<MetaData version=\"1.0.0\"><Struct name=\"KeyedSeq\"><Member name=\"seq\"><ULong/></Member><Member name=\"keyval\"><Long/></Member><Member name=\"baggage\"><Sequence><Octet/></Sequence></Member></Struct></MetaData>"

-- Using the generic copy routines to keep life easy.  One can also
-- run idlpp, turn the generated code into a C library and call
-- "KeyedSeqTypeSupport__alloc" for newTypeSupport, eliminating the
-- additional overhead of the C "generic" copy routines.
instance NewTopicClass KeyedSeq where
  newTypeSupport _ = R.genericTypeSupportAlloc "KeyedSeq" "keyval" metaData
