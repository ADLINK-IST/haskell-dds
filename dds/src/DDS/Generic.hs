{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, DefaultSignatures  #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module DDS.Generic where

import GHC.Generics

import DDS.Core
import qualified DDS.Type as U
import qualified DDS.Raw as R

import Data.Aeson
import Data.Aeson.Types (Options(..), camelTo2, emptyObject, parseMaybe)
import qualified Data.Text as T
import qualified Data.Scientific as Scientific
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import Control.Monad (liftM)

import Data.Int
import Data.Word
import Data.Maybe
import Data.Char (toLower)
import Data.List (stripPrefix, elemIndex, sort)

import Foreign.C
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

apokeInt :: Ptr a -> U.Type -> Scientific.Scientific -> IO ()
apokeInt ptr (U.TInt iw sgn) v =
  case (iw, sgn) of
    (U.I1, U.Signed)   -> store v (castPtr ptr :: Ptr Int8)
    (U.I1, U.Unsigned) -> store v (castPtr ptr :: Ptr Word8)
    (U.I2, U.Signed)   -> store v (castPtr ptr :: Ptr Int16)
    (U.I2, U.Unsigned) -> store v (castPtr ptr :: Ptr Word16)
    (U.I4, U.Signed)   -> store v (castPtr ptr :: Ptr Int32)
    (U.I4, U.Unsigned) -> store v (castPtr ptr :: Ptr Word32)
    (U.I8, U.Signed)   -> store v (castPtr ptr :: Ptr Int64)
    (U.I8, U.Unsigned) -> store v (castPtr ptr :: Ptr Word64)
  where
    store :: (Integral a, Bounded a, Storable a) => Scientific.Scientific -> Ptr a -> IO ()
    store v ptr = do
      let mv = Scientific.toBoundedInteger v
      case mv of
        Nothing -> fail $ "value " ++ show v ++ " out of range of Int8"
        Just x  -> poke ptr x

apokeType :: Ptr a -> U.Type -> Value -> (Ptr a -> IO b) -> IO b
apokeType ptr tt val fun =
  let ptr' = alignPtr ptr a
      ptr'' = ptr' `plusPtr` sz
      (sz,a) = R.calcSizeAlign tt (0,1) in
  case (tt,val) of
    (U.TChar, Number v) -> do
      apokeInt ptr' (U.TInt U.I1 U.Unsigned) v
      fun ptr''
    (U.TChar, String v) -> do
      poke (castPtr ptr') (((fromIntegral . fromEnum . T.head) v) :: Int8)
      fun ptr''
    (U.TBool, Bool v) -> do
      poke (castPtr ptr') (((fromIntegral . fromEnum) v) :: Word8)
      fun ptr''
    (U.TTime, Number v) -> do -- with an ugly hack to allow setting the msb of the ns part
      let b = 1000000000 :: Integer
          mv = round (v * fromIntegral b) :: Integer
          z = mv `div` b
          s = fromIntegral (z `rem` (10*b)) :: Int32
          ns = fromIntegral ((mv `rem` b) + (z `div` (10*b))) :: Int32
      poke (castPtr ptr') s
      poke (castPtr $ plusPtr ptr' 4) ns
      fun ptr''
    (U.TInt _ _, Number v) -> do
      apokeInt ptr' tt v
      fun ptr''
    (U.TFloat fw, Number v) -> do
      case fw of
        U.F4 -> poke (castPtr ptr') (Scientific.toRealFloat v :: Float)
        U.F8 -> poke (castPtr ptr') (Scientific.toRealFloat v :: Double)
      fun ptr''
    (U.TEnum _ _, Number v) -> do
      apokeInt ptr' (U.TInt U.I4 U.Unsigned) v
      fun ptr''
    (U.TEnum _ es, String v) -> do
      let Just ix = M.lookup v es
      poke (castPtr ptr') ix
      fun ptr''
    (U.TString _, String v) -> do
      withCString (T.unpack v) $ \str -> do
        poke (castPtr ptr') str
        fun ptr''
    (U.TSequence _ t, Array vs) -> do
      let (tsz, ta) = R.calcSizeAlign t (0,1)
          n = length vs
      allocaBytes (n * tsz) $ \seqptr -> do
        poke (castPtr ptr') $ R.Sequence n n False seqptr
        apokeSeq seqptr t n (V.toList vs) $ \_ -> fun ptr'' -- FIXME: use Vector all the way
    (U.TArray n t, Array vs) -> do
      let (tsz, ta) = R.calcSizeAlign t (0,1)
          tail = repeat (apokeDefault t)
      apokeSeq ptr' t n (V.toList vs++tail) $ \_ -> fun ptr'' -- FIXME: use Vector all the way
    (U.TStruct ms _, Object vs) -> do
      apokeStruct ptr' ms vs $ \_ -> fun ptr''
    (U.TUnion dt coff cs cmap cdef, Object vs) -> do
      let Just disc = getDisc dt cs vs
          Just contents = parseMaybe (.: "contents") vs -- lazy, so no contents is ok if none are needed
      apokeType ptr' dt disc $ \_ -> do
        let cptr' = ptr' `plusPtr` coff
            cnumval = convDisc dt disc
            c = case M.lookup cnumval cmap of Just x -> Just x ; Nothing -> cdef
        case c of
          Nothing -> fun ptr''
          Just (_,t) -> apokeType cptr' t contents $ \_ -> fun ptr''
    (t, Null) -> apokeType ptr t (apokeDefault t) fun

convDisc U.TBool (Bool v) = fromIntegral $ fromEnum v
convDisc (U.TEnum _ m) (String v) = fromIntegral $ m M.! v
convDisc (U.TEnum _ m) (Number v) = floor v
convDisc (U.TInt _ _) (Number v) = floor v

getDisc :: U.Type -> [U.UnionCase] -> Object -> Maybe Value
getDisc dt cs vs
  | isJust disc = disc
  | isJust tag = lookupTag $ fromJust tag
  | otherwise = Nothing
  where
    tagTable = map (\(ls,nm,_) -> (nm,ls)) cs
    unused = findUnused dt $ findUsed cs
    findUsed = sort . catMaybes . concatMap (\(ls,_,_) -> ls)
    disc = parseMaybe (.: "disc") vs
    tag = parseMaybe (.: "tag") vs
    lookupTag tag
      | isNothing t = Nothing
      | null vs = Just $ r unused
      | otherwise = Just $ r $ head vs
      where
        t = lookup tag tagTable
        vs = catMaybes $ fromJust t
        r = \v -> Number (Scientific.scientific v 0)

-- find an value that is not in the provided set of covered values
-- the assumption is that there is an unused value that is in range
findUnused :: U.Type -> [Integer] -> Integer
findUnused (U.TInt w s) vs
  | null vs = 0
  | head vs > a = head vs - 1
  | otherwise = ffu vs
  where
    a = case s of
      U.Signed -> case w of U.I1 -> -2^7 ; U.I2 -> -2^15 ; U.I4 -> -2^31 ; U.I8 -> -2^63
      U.Unsigned -> 0
    ffu [v] = v + 1
    ffu (v:vs) = ffu' v vs
    ffu' u [] = u + 1
    ffu' u (v:vs) = if u + 1 < v then u + 1 else ffu' v vs
findUnused (U.TEnum _ _) vs = findUnused (U.TInt U.I8 U.Unsigned) vs
findUnused U.TBool vs = findUnused (U.TInt U.I1 U.Unsigned) vs

apokeSeq :: Ptr a -> U.Type -> Int -> [Value] -> (Ptr a -> IO b) -> IO b
apokeSeq ptr tt 0 _ fun = fun ptr
apokeSeq ptr tt n (v:vs) fun = apokeType ptr tt v (\ptr' -> apokeSeq ptr' tt (n-1) vs fun)

apokeStruct :: Ptr a -> [(T.Text,U.Type)] -> M.HashMap T.Text Value -> (Ptr a -> IO b) -> IO b
apokeStruct ptr [] _ fun = fun ptr
apokeStruct ptr ((nm,t):ms) vs fun =
  let v = fromMaybe (apokeDefault t) (M.lookup nm vs)
   in apokeType ptr t v (\ptr' -> apokeStruct ptr' ms vs fun)

apokeDefault :: U.Type -> Value
apokeDefault (U.TChar) = Number 0
apokeDefault (U.TBool) = Bool False
apokeDefault (U.TInt _ _) = Number 0
apokeDefault (U.TFloat _) = Number 0
apokeDefault (U.TString _) = String ""
apokeDefault (U.TSequence _ _) = Array V.empty
apokeDefault (U.TTime) = Number 0
apokeDefault (U.TArray _ _) = Array V.empty
apokeDefault (U.TStruct _ _) = Object M.empty
apokeDefault (U.TUnion _ _ _ _ _) = Object M.empty
apokeDefault (U.TEnum _ _) = Number 0

withSampleJSON :: U.TopicType -> Value -> (Ptr a -> IO b) -> IO b
withSampleJSON tt val fun =
  let sz = U.topicTypeSizeof tt
      tp = U.topicTypeType tt
   in allocaBytes sz $ \ptr -> apokeType ptr tp val (\_ -> fun ptr)

apeekInt :: Integral a => a -> Scientific.Scientific
apeekInt = (flip Scientific.scientific) 0 . fromIntegral

apeekType :: U.Type -> Ptr a -> IO (Value, Ptr a)
apeekType tt ptr =
  let ptr' = alignPtr ptr a
      ptr'' = ptr' `plusPtr` sz
      (sz,a) = R.calcSizeAlign tt (0,1) in
  case tt of
    U.TChar -> do
      v <- liftM (T.singleton . toEnum . fromIntegral) $ (peek (castPtr ptr') :: IO Int8)
      return (String v, ptr'')
    U.TBool -> do
      v <- liftM (toEnum . fromIntegral) $ (peek (castPtr ptr') :: IO Word8)
      return (Bool v, ptr'')
    (U.TTime) -> do
      s <- liftM fromIntegral $ (peek (castPtr ptr') :: IO Int32)
      ns <- liftM fromIntegral $ (peek (castPtr $ plusPtr ptr' 4) :: IO Int32)
      let ts = fromIntegral s * (1000000000 :: Integer) + fromIntegral ns
      return (Number $ Scientific.scientific ts (-9), ptr'')
    (U.TInt iw sgn) -> do
      v <- case (iw, sgn) of
        (U.I1, U.Signed)   -> liftM apeekInt $ (peek (castPtr ptr') :: IO Int8)
        (U.I1, U.Unsigned) -> liftM apeekInt $ (peek (castPtr ptr') :: IO Word8)
        (U.I2, U.Signed)   -> liftM apeekInt $ (peek (castPtr ptr') :: IO Int16)
        (U.I2, U.Unsigned) -> liftM apeekInt $ (peek (castPtr ptr') :: IO Word16)
        (U.I4, U.Signed)   -> liftM apeekInt $ (peek (castPtr ptr') :: IO Int32)
        (U.I4, U.Unsigned) -> liftM apeekInt $ (peek (castPtr ptr') :: IO Word32)
        (U.I8, U.Signed)   -> liftM apeekInt $ (peek (castPtr ptr') :: IO Int64)
        (U.I8, U.Unsigned) -> liftM apeekInt $ (peek (castPtr ptr') :: IO Word64)
      return (Number v, ptr'')
    (U.TFloat fw) -> do
      v <- case fw of
        U.F4 -> liftM Scientific.fromFloatDigits $ (peek (castPtr ptr') :: IO Float)
        U.F8 -> liftM Scientific.fromFloatDigits $ (peek (castPtr ptr') :: IO Double)
      return (Number v, ptr'')
    (U.TEnum es _) -> do
      ixw32 <- peek (castPtr ptr' :: Ptr Word32)
      let ix = fromIntegral ixw32
      return (String (es V.! ix), ptr'')
    (U.TString _) -> do
      str <- peek (castPtr ptr') :: IO CString
      v <- if str == nullPtr then return "" else peekCString str
      return (String $ T.pack v, ptr'')
    (U.TSequence _ t) -> do
      let (tsz, ta) = R.calcSizeAlign t (0,1)
      seq <- (peek (castPtr ptr') :: IO (R.Sequence a))
      let n = R.seqLength seq
      vs <- apeekSeq t (R.seqBuffer seq) n
      return (Array $ V.fromList vs, ptr'')
    (U.TArray n t) -> do
      let (tsz, ta) = R.calcSizeAlign t (0,1)
      let tail = repeat (apokeDefault t)
      vs <- apeekSeq t ptr' n
      return (Array $ V.fromList vs, ptr'')
    (U.TStruct ms _) -> do
      vs <- apeekStruct ms ptr
      return (Object $ M.fromList vs, ptr'')
    (U.TUnion dt coff cs cmap cdef) -> do
      (dv, _) <- apeekType dt ptr'
      let cptr' = ptr' `plusPtr` coff
          cnumval = convDisc dt dv
          c = case M.lookup cnumval cmap of
                Just x -> Just x
                Nothing -> cdef
      case c of
        Nothing -> return (Object $ M.fromList [("disc",dv)], ptr'')
        Just (n,t) -> do
          (v, _) <- apeekType t cptr'
          return (Object $ M.fromList [("disc",dv), ("tag",String n), ("contents",v)], ptr'')

apeekSeq :: U.Type -> Ptr a -> Int -> IO [Value]
apeekSeq tt ptr n | n <= 0    = return []
                  | otherwise = f (n-1) (ptr `plusPtr` ((n-1) * sz)) []
  where
    (sz, _) = R.calcSizeAlign tt (0,1)
    f 0 p acc = do (v, _) <- apeekType tt p ; return (v:acc)
    f n p acc = do (v, _) <- apeekType tt p ; f (n-1) (p `plusPtr` (-sz)) (v:acc)

apeekStruct :: [(T.Text,U.Type)] -> Ptr a -> IO [(T.Text, Value)]
apeekStruct ms p = f ms p
  where
    f [] _ = return []
    f ((nm,t):ms) p = do (v, p') <- apeekType t p ; rest <- f ms p' ; return $ (nm,v):rest

peekSampleJSON :: U.TopicType -> Ptr Value -> IO Value
peekSampleJSON tt ptr = do (v, _) <- apeekType (U.topicTypeType tt) ptr ; return v

instance R.TopicClass Value where
  withSample = withSampleJSON
  peekSample = peekSampleJSON
  sizeofSample t = R.SampleSize (U.topicTypeSizeof t)
  
instance {-# OVERLAPPABLE #-} (ToJSON a, FromJSON a) => R.TopicClass a where
  withSample tt v fun = withSampleJSON tt (toJSON v) fun
  peekSample tt ptr = do
    v <- peekSampleJSON tt (castPtr ptr)
    let v' = fromJSON v
    case v' of
      Error msg -> fail msg
      Success x -> return x
  sizeofSample t = R.SampleSize (U.topicTypeSizeof t)
