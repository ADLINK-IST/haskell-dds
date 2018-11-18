{-# LANGUAGE DeriveGeneric, TemplateHaskell, CPP #-}

module DDS.TH (topicidl, topicidls) where

import Data.Aeson
import Data.Aeson.Types (Options(..))
import qualified Data.Char as Char
import Data.Maybe

import GHC.Generics

import DDS.Core
import DDS.Raw (genericTypeSupportAlloc)
import DDS.Generic

import Debug.Trace

import Data.Int
import Data.Word
import Data.List
import Data.List.Split
import DDS.TopicXML
import DDS.IDL (parseIDL)
import Data.Maybe (fromJust, fromMaybe)

import Language.Haskell.TH
import Language.Haskell.TH.Quote
#if __GLASGOW_HASKELL__ >= 800
import Language.Haskell.TH as TH
#endif

import GHC.Show
import GHC.Classes

{-
runQ [d| data Throughput = Throughput { tpCount :: Integer, tpPayload :: [Word8] } deriving (Show, Eq, Generic) |]

runQ [d| data Un = Un0 { unX :: Integer } | Un1 { unY :: Bool } | Un deriving (Show, Eq, Generic) |]

-->

[DataD [] Un_0 [] Nothing [
   RecC Un0_1 [(unX_4,Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Integer.Type.Integer)],
   RecC Un1_2 [(unY_5,Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Types.Bool)],
   NormalC Un_3 []] [ConT GHC.Show.Show,ConT GHC.Classes.Eq,ConT GHC.Generics.Generic]]

====

runQ [d| data Un = UnX Integer | UnY Bool | Un deriving (Show, Eq, Generic) |]

-->

[DataD [] Un_6 [] Nothing [
  NormalC UnX_7 [(Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Integer.Type.Integer)],
  NormalC UnY_8 [(Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Types.Bool)],
  NormalC Un_9 []] [ConT GHC.Show.Show,ConT GHC.Classes.Eq,ConT GHC.Generics.Generic]]

-}

cap1st :: String -> String
cap1st [] = []
cap1st (c:cs) | Char.isUpper c = c:cs
              | otherwise = Char.toUpper c : cs
uncap1st :: String -> String
uncap1st [] = []
uncap1st (c:cs) | Char.isLower c = c:cs
                | otherwise = Char.toLower c : cs

cookname :: String -> String
cookname n
  | head cn == '_' = tail cn
  | otherwise = cn
  where
    cn = intercalate "_" $ splitOn "::" n

cooknameU = cap1st . cookname
cooknameL = uncap1st . cookname

#if __GLASGOW_HASKELL__ >= 800
notstrict = Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
#else
notstrict = NotStrict
#endif

defname (DT nm _) = nm
defname (DS nm _) = nm
defname (DE nm _) = nm
defname (DU nm _ _) = nm
defname x = error "defname called with an unexpected type " ++ show x

togenDef :: Scope -> Maybe Def -> [(Scope, Def)]
togenDef _ Nothing = []
togenDef sc (Just d) = c d
  where
    c (DT nm t)       = togenT (sc' nm) t
    c d@(DS nm ms)    = concatMap (\(M _ t) -> togenT (sc' nm) t) ms ++ [(sc, d)]
    c d@(DE nm es)    = [(sc, d)]
    c d@(DU nm dt cs) = concatMap (\(UC _ t _) -> togenT (sc' nm) t) cs ++ togenT (sc' nm) dt ++ [(sc, d)]
    c _               = []
    sc' nm = fromMaybe sc $ lookup nm (scSubscopes sc)

togenT :: Scope -> T -> [(Scope, Def)]
togenT _ (TP _) = []
togenT _ (TStr _) = []
togenT sc (TSeq _ t) = togenT sc t
togenT sc (TArr _ t) = togenT sc t
togenT sc (TR nm)    = uncurry togenDef $ lookupDef nm sc
togenT sc (TDef def) = togenDef sc (Just def)

togen :: Scope -> String -> [(Scope, Def)]
togen sc typename = uncurry togenDef $ lookupDef typename sc

makeConstructorS n
  | isJust hn = ConT $ mkName $ intercalate "." $ splitOn "::" $ fromJust hn
  | otherwise = ConT $ mkName $ cooknameU n
  where
    hn = stripPrefix "::Haskell::" n

makeConstructor' :: String -> T -> (Language.Haskell.TH.Name, Strict, Type)
makeConstructor' name ty = (mkName name, notstrict, hty ty)
  where
    hty ty = case ty of
      TP TC  -> ConT ''Char
      TP TB  -> ConT ''Bool
      TP TO  -> ConT ''Word8
      TP TS  -> ConT ''Int
      TP TUS -> ConT ''Int
      TP TF  -> ConT ''Double
      TP TD  -> ConT ''Double
      TP _   -> ConT ''Integer
      TStr _ -> ConT ''String
      TSeq _ ty' -> list ty'
      TArr _ ty' -> list ty'
      TR nm -> makeConstructorS nm
      TDef (DT nm _) -> ConT $ mkName $ cooknameU nm
      TDef (DS nm _) -> ConT $ mkName $ cooknameU nm
      TDef (DE nm _) -> ConT $ mkName $ cooknameU nm
    list ty = AppT ListT $ hty ty

makeConstructor :: String -> Scope -> M -> (Language.Haskell.TH.Name, Strict, Type)
makeConstructor pfx sc (M name ty) = makeConstructor' (pfx ++ cap1st name) ty

-- ccs: [NormalC (tn'++tagname) cs, ...]
-- deriv = [''Generic] or deriv = [''Generic, ''Show]
#if __GLASGOW_HASKELL__ >= 802
mkStructDataD deriv tn' cs = [DataD [] tn' [] Nothing [RecC tn' cs] [DerivClause Nothing (map ConT deriv)]]
mkUnionDataD deriv tn' ccs = [DataD [] tn' [] Nothing ccs [DerivClause Nothing (map ConT deriv)]]
#elif __GLASGOW_HASKELL__ >= 800
mkStructDataD deriv tn' cs = [DataD [] tn' [] Nothing [RecC tn' cs] (map ConT deriv)]
mkUnionDataD deriv tn' ccs = [DataD [] tn' [] Nothing ccs (map ConT deriv)]
#else
mkStructDataD deriv tn' cs = [DataD [] tn' [] [RecC tn' cs] deriv]
mkUnionDataD deriv tn' ccs = [DataD [] tn' [] ccs deriv]
#endif

genStruct :: [TH.Name] -> String -> Scope -> Def -> [Dec]
genStruct deriv pfx sc (DS tn ms) = mkStructDataD deriv tn' cs
  where
    cs = map (makeConstructor pfx sc) ms
    tn' = mkName $ cooknameU tn

overridePrefix :: String -> Scope -> String -> String
overridePrefix pfx sc nm
  -- | (isNothing . scParent) sc = pfx
  | isJust mpfx' = pfx'
  | otherwise = pfx
  where
    (_, mpfx') = lookupDef (nm ++ "'Prefix") sc
    Just (DP _ pfx') = mpfx'

genUnion :: [TH.Name] -> String -> Scope -> Def -> [Dec]
genUnion deriv pfx sc (DU tn dt cs) = mkUnionDataD deriv tn' ccs
  where
    ccs = map (\(nm,st,ty) -> NormalC nm [(st,ty)]) $ map (\(UC nm ty _) -> makeConstructor' (ctn ++ nm) ty) cs
    ctn = cooknameU tn
    tn' = mkName ctn

genEnum :: [TH.Name] -> String -> Scope -> Def -> [Dec]
genEnum deriv pfx sc (DE tn es) =
#if __GLASGOW_HASKELL__ >= 802
  [DataD [] tn' [] Nothing cs [DerivClause Nothing (map ConT (deriv ++ [''Enum, ''Eq, ''Ord]))]]
#else
  [DataD [] tn' [] Nothing cs (map ConT (deriv ++ [''Enum, ''Eq, ''Ord]))]
#endif
  where
    -- FIXME: ignoring numerical values
    cs = map (\(E nm _) -> NormalC (mkName $ cap1st nm) []) es
    tn' = mkName $ cooknameU tn

genDef :: [TH.Name] -> String -> Scope -> Def -> [Dec]
genDef deriv pfx sc def@(DS tn _) = genStruct deriv pfx' sc def ++ genSerdes pfx' tn
  where
    pfx' = overridePrefix pfx sc tn
genDef deriv pfx sc def@(DE tn _) = genEnum deriv "" sc def ++ genSerdes "" tn
genDef deriv pfx sc def@(DU tn _ _) = genUnion deriv pfx sc def ++ genSerdes pfx tn

genSerdes :: String -> String -> [Dec]
genSerdes pfx tn =
#if __GLASGOW_HASKELL__ >= 800
  [InstanceD Nothing [] (AppT (ConT ''ToJSON) (ConT tn')) [ValD (VarP 'toJSON) (NormalB (AppE (VarE 'genericToJSON) opts)) []], InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT tn')) [ValD (VarP 'parseJSON) (NormalB (AppE (VarE 'genericParseJSON) opts)) []]]
#else
  [InstanceD [] (AppT (ConT ''ToJSON) (ConT tn')) [ValD (VarP 'toJSON) (NormalB (AppE (VarE 'genericToJSON) opts)) []], InstanceD [] (AppT (ConT ''FromJSON) (ConT tn')) [ValD (VarP 'parseJSON) (NormalB (AppE (VarE 'genericParseJSON) opts)) []]]
#endif
  where
    tn' = mkName $ cooknameU $ (head . reverse . splitOn "::") tn
    opts = RecUpdE (VarE 'defaultOptions) [('fieldLabelModifier, InfixE (Just (VarE 'uncap1st)) (VarE '(.)) (Just (AppE (VarE 'drop) (LitE (IntegerL $ (fromIntegral.length) pfx))))), ('constructorTagModifier, InfixE (Just (VarE 'id)) (VarE '(.)) (Just (AppE (VarE 'drop) (LitE (IntegerL $ (fromIntegral.length) pfx)))))]

newTopicDef :: String -> TopicMD -> [Dec]
newTopicDef tn md =
#if __GLASGOW_HASKELL__ >= 800
  [InstanceD Nothing [] (AppT (ConT ''NewTopicClass) (ConT tn')) [FunD 'newTypeSupport [Clause [WildP] (NormalB (AppE (AppE (AppE (VarE 'genericTypeSupportAlloc) (LitE (StringL $ topicMDTypename md))) (LitE (StringL $ topicMDKeylist md))) (LitE (StringL $ topicMDMetadata md)))) []]]]
#else
  [InstanceD [] (AppT (ConT ''NewTopicClass) (ConT tn')) [FunD 'newTypeSupport [Clause [WildP] (NormalB (AppE (AppE (AppE (VarE 'genericTypeSupportAlloc) (LitE (StringL $ topicMDTypename md))) (LitE (StringL $ topicMDKeylist md))) (LitE (StringL $ topicMDMetadata md)))) []]]]
#endif
  where 
    tn' = mkName $ cooknameU $ (head . reverse . splitOn "::") tn

prefixFromName :: String -> String
prefixFromName = map Char.toLower . head . reverse . splitOn "::"

getPrefix :: String -> (String -> String, String)
getPrefix idl
  | null prefix || null rest || head rest /= '|' = (prefixFromName, idl)
  | otherwise = (const prefix, tail rest)
  where
    (prefix, rest) = span Char.isAlphaNum idl

fromIDLwithDeriv :: [TH.Name] -> String -> Q [Dec]
fromIDLwithDeriv deriv idl = return $ concatMap dodef (defs tps) ++ concatMap dotopic tps
  where
    tps = topicsFromMetaData md
    md = case parseIDL idl' of
      Right x -> x
      Left err -> error err
    (pfxF, idl') = getPrefix idl
    defs tps = nubBy (\(_,a) (_,b) -> a == b) $ concatMap scopes_defs tps
      where
        scopes_defs tmd = togen sc tn
          where
            tn = topicMDTypename tmd
            (sc, _) = lookupDef tn $ cMD md
    dodef (scope,def) = genDef deriv (pfx def) scope def
      where pfx = pfxF . defname
    --pfx = (pfxF . topicMDTypename . head) tps
    dotopic tmd = newTopicDef tn tmd
      where
        tn = topicMDTypename tmd

fromIDL :: String -> Q [Dec]
fromIDL = fromIDLwithDeriv [''Generic]

fromIDLwithShow :: String -> Q [Dec]
fromIDLwithShow = fromIDLwithDeriv [''Generic, ''Show]

topicidl :: QuasiQuoter
topicidl = QuasiQuoter { quoteDec = fromIDL, quoteExp = undefined, quotePat = undefined, quoteType = undefined }

topicidls :: QuasiQuoter
topicidls = QuasiQuoter { quoteDec = fromIDLwithShow, quoteExp = undefined, quotePat = undefined, quoteType = undefined }
