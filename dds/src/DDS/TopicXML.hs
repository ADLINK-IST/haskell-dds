module DDS.TopicXML (topicsFromMetaData, convFromXML, convToXML, lookupDef, getTopicType, cMD, TopicMD(..), MetaData(..), Name, Def(..), E(..), M(..), UC(..), UL(..), TP(..), T(..), Scope(..)) where

import Text.XML.HXT.Core hiding (lookupDef)
import Data.Maybe
import Data.List (intercalate)
import Data.List.Split (splitOn)

import DDS.TopicMDOverrides

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

import qualified DDS.Type as U
import qualified DDS.Raw as R (calcSizeAlign)

data TopicMD = TopicMD { topicMDTypename, topicMDKeylist, topicMDMetadata :: String } deriving (Show, Eq)

convFromXML = fromJust . head . runLA (xread >>> arr (unpickleDoc xpMetaData))
convToXML   = convToXML' . sanitizeMetaData
convToXML'  = head . runLA (xshow (arr (pickleDoc xpMetaData) >>> getChildren))

---------------------------------------------

topicsFromMetaData :: MetaData -> [TopicMD]
topicsFromMetaData md = concatMap (fDef "" sc) ds
  where
    sc = cMD md
    MD ds = md
    --
    fDef :: String -> Scope -> Def -> [TopicMD]
    fDef pr sc (DT n t)  = fT (pr++"::"++n) (fst $ lookupDef n sc) t
    fDef pr sc (DM n ds) = concatMap (fDef (pr++"::"++n) (fst $ lookupDef n sc)) ds
    fDef pr sc (DS n ms) = concatMap (fM (pr++"::"++n) (fst $ lookupDef n sc)) ms
    fDef pr sc (DU n dt cs) = []
    fDef pr sc (DE n es) = []
    fDef pr sc (DK n ks) = [makeTopicMD pr sc n ks]
    fDef pr sc (DC n v)  = []
    fDef pr sc (DP n pfx) = []
    fT :: String -> Scope -> T -> [TopicMD]
    fT pr sc (TDef d)    = fDef pr sc d
    fT pr sc (TSeq b t)  = fT pr sc t
    fT pr sc (TArr b t)  = fT pr sc t
    fT _  _  _           = []
    fM :: String -> Scope -> M -> [TopicMD]
    fM pr sc (M _ t) = fT pr sc t
    --
    makeTopicMD pr sc tn ks = TopicMD
      { topicMDTypename = drop 2 $ pr++"::"++tn -- need fully qualified, but not absolute
      , topicMDKeylist  = intercalate "," ks
      , topicMDMetadata = convToXML md -- should do only types required for tn
      }

---------------------------------------------

makeUnionMap :: [U.UnionCase] -> (M.HashMap Integer (T.Text,U.Type), Maybe (T.Text,U.Type))
makeUnionMap cases = foldl f (M.empty, Nothing) cases
  where
    f acc (ls,n,t) = foldl (g n t) acc ls
    g n t (m,_) Nothing = (m, Just (n,t))
    g n t (m,d) (Just v) = (M.insert v (n,t) m, d)

convDef :: Scope -> Maybe Def -> Maybe U.Type
convDef _ Nothing = Nothing
convDef sc (Just d) = c d
  where
    c (DT nm t) = convT (sc' nm) t
    c (DS nm ms) = do
      ts <- traverse (convT (sc' nm)) (map tM ms)
      let xs = zip (map nM ms) ts
      return $ U.TStruct xs (M.fromList xs)
    c (DU nm dt cs) = do
      dt' <- convT (sc' nm) dt
      cs' <- mapM (convUC (sc' nm) dt') cs
      let (cm', dc') = makeUnionMap cs'
          (sd, _) = R.calcSizeAlign dt' (0,1)
          ca = maximum $ map snd $ map (\(_,_,t) -> R.calcSizeAlign t (0,1)) cs'
          pad a n = if (n `mod` a) == 0 then n else n + a - (n `mod` a)
          off' = pad ca sd
      return $ U.TUnion dt' off' cs' cm' dc'
    c (DE nm es) = do
      let xs = map nE es
      return $ U.TEnum (V.fromList $ map fst xs) (M.fromList xs)
    c _ = Nothing
    nE (E n v) = (T.pack n, v)
    nM (M n _) = T.pack n
    tM (M _ t) = t
    sc' nm = fromMaybe sc $ lookup nm (scSubscopes sc)

convUC :: Scope -> U.Type -> UC -> Maybe U.UnionCase
convUC sc dt (UC nm t ls) = do
  t' <- convT (sc' nm) t
  ls' <- mapM (convUL dt) ls
  return $ (ls', T.pack nm, t')
  where
    boole = (U.TEnum undefined $ M.fromList [(T.pack "false",0), (T.pack "true",1)])
    convUL _ ULDef = return $ Nothing
    convUL (U.TEnum _ es) (UL v) = do
      v' <- M.lookup (T.pack v) es
      return $ Just (fromIntegral v')
    convUL U.TBool (UL v) = convUL boole (UL v)
    convUL _ (UL v) = return $ Just (read v)
    sc' nm = fromMaybe sc $ lookup nm (scSubscopes sc)

convT :: Scope -> T -> Maybe U.Type
convT _ (TP tp) = return $ case tp of
    TC   -> U.TChar
    TB   -> U.TBool
    TO   -> U.TInt U.I1 U.Unsigned
    TS   -> U.TInt U.I2 U.Signed
    TUS  -> U.TInt U.I2 U.Unsigned
    TL   -> U.TInt U.I4 U.Signed
    TUL  -> U.TInt U.I4 U.Unsigned
    TLL  -> U.TInt U.I8 U.Signed
    TULL -> U.TInt U.I8 U.Unsigned
    TF   -> U.TFloat U.F4
    TD   -> U.TFloat U.F8
    TT   -> U.TTime
convT _ (TStr b)    = return $ U.TString b
convT sc (TSeq b t) = do t' <- convT sc t ; return $ U.TSequence b t'
convT sc (TArr n t) = do t' <- convT sc t ; return $ U.TArray n t'
convT sc (TR nm)    = uncurry convDef $ lookupDef nm sc
convT sc (TDef def) = convDef sc (Just def)

getType :: String -> String -> Maybe U.Type
getType metadata typename = uncurry convDef $ lookupDef typename scope
  where
    scope = cMD $ convFromXML metadata

getTopicType :: String -> String -> Maybe U.TopicType
getTopicType md tn =
  let
    md' = topicMetaDescriptionOverride tn md
    tn' = topicTypenameOverride tn
  in case getType md' tn' of
    Nothing -> Nothing
    (Just typ) ->
      let (siz, alg) = R.calcSizeAlign typ (0,1)
      in Just $ U.TopicType { U.topicTypeName = tn', U.topicTypeSizeof = siz, U.topicTypeAlignof = alg, U.topicTypeType = typ }

-- should strip unneeded ones
-- should find out whether to use fq names, absolute refs or others
-- etc. etc.

---------------------------------------------

-- removes consts, keylists
-- perhaps eventually expressions should be allowed, to be eval'd here
sanitizeMetaData :: MetaData -> MetaData
sanitizeMetaData (MD ds) = MD $ mapMaybe sanDef ds
  where
    sanDef :: Def -> Maybe Def
    sanDef (DT n t)  = Just $ DT n $ sanT t
    sanDef (DM n ds) = Just $ DM n $ mapMaybe sanDef ds
    sanDef (DS n ms) = Just $ DS n $ map sanM ms
    sanDef (DU n dt cs) = Just $ DU n dt $ map sanUC cs
    sanDef (DE n es) = Just $ DE n es
    sanDef _         = Nothing
    sanT :: T -> T
    sanT (TDef d)    = TDef $ fromJust $ sanDef d
    sanT (TSeq b t)  = TSeq b $ sanT t
    sanT (TArr b t)  = TArr b $ sanT t
    sanT x           = x
    sanM :: M -> M
    sanM (M n t) = M n $ sanT t
    sanUC :: UC -> UC
    sanUC (UC n t ls) = UC n (sanT t) ls

---------------------------------------------
---
---  XML Picklers
---
---------------------------------------------

data MetaData = MD [Def] deriving (Show, Eq)
xpMetaData :: PU MetaData
xpMetaData = xpWrap (MD, \(MD ds) -> ds) $
             xpElem "MetaData" $ xpAddFixedAttr "version" "1.0.0" $ xpickle

type Name = String
data Def = DT Name T | DM Name [Def] | DS Name [M] | DU Name T [UC] | DE Name [E] | DC Name Int | DK Name [Name] | DP Name Name deriving (Show, Eq)
instance XmlPickler Def where
  xpickle = xpAlt tag ps
    where
      tag (DT _ _) = 0
      tag (DM _ _) = 1
      tag (DS _ _) = 2
      tag (DU _ _ _) = 3
      tag (DE _ _) = 4
      tag (DC _ _) = 5 -- Const isn't part of OSPL meta data, temp for getting enum const in Scope
      tag (DK _ _) = 6 -- Keylist isn't part, but should've been :) (certainly makes parsing IDL easier)
      tag (DP _ _) = 7 -- Field prefix (Haskell specific)
      ps = [ xpWrap (uncurry DT, \(DT nm t) -> (nm, t)) $
             (xpElem "TypeDef" $ xpPair (xpAttr "name" xpText) xpickle)
           , xpWrap (uncurry DM, \(DM nm ds) -> (nm, ds)) $
             (xpElem "Module" $ xpPair (xpAttr "name" xpText) $ xpList xpickle)
           , xpWrap (uncurry DS, \(DS nm ms) -> (nm, ms)) $
             (xpElem "Struct" $ xpPair (xpAttr "name" xpText) $ xpList xpickle)
           , xpWrap (\(nm, dt, cs) -> DU nm dt cs, \(DU nm dt cs) -> (nm, dt, cs)) $
             (xpElem "Union" $ xpTriple (xpAttr "name" xpText) (xpElem "SwitchType" xpickle) (xpList xpickle))
           , xpWrap (uncurry DE, \(DE nm es) -> (nm, es)) $
             (xpElem "Enum" $ xpPair (xpAttr "name" xpText) $ xpList xpickle)
           , xpWrap (uncurry DC, \(DC nm v) -> (nm, v)) $
             (xpElem "Const" $ xpPair (xpAttr "name" xpText) $ xpickle)
           , xpWrap (\(nm, ks) -> DK nm (splitOn "," ks), \(DK nm ks) -> (nm, intercalate "," ks)) $
             (xpElem "KeyList" $ xpPair (xpAttr "name" xpText) (xpAttr "keys" xpText))
           , xpWrap (\(nm, pfx) -> DP nm pfx, \(DP nm pfx) -> (nm, pfx)) $
             (xpElem "FieldPrefix" $ xpPair (xpAttr "name" xpText) (xpAttr "prefix" xpText))
           ]

data E = E Name Int deriving (Show, Eq)
instance XmlPickler E where
  xpickle = xpWrap (uncurry E, \(E nm v) -> (nm, v)) $
            xpElem "Element" $ xpPair (xpAttr "name" xpText) (xpAttr "value" xpickle)

data M = M Name T deriving (Show, Eq)
instance XmlPickler M where
  xpickle = xpWrap (uncurry M, \(M nm t) -> (nm, t)) $
            xpElem "Member" $ xpPair (xpAttr "name" xpText) xpickle

data UC = UC Name T [UL] deriving (Show, Eq)
instance XmlPickler UC where
  xpickle = xpWrap (\(nm, t, ls) -> UC nm t ls, \(UC nm t ls) -> (nm, t, ls)) $
            xpElem "Case" $ xpTriple (xpAttr "name" xpText) xpickle (xpList xpickle)

data UL = UL String | ULDef deriving (Show, Eq)
instance XmlPickler UL where
  xpickle = xpAlt tag ps
    where
        tag (UL _) = 0
        tag ULDef = 1
        ps = [ xpWrap (UL, \(UL v) -> v) $
               xpElem "Label" $ xpAttr "value" xpText
             , xpWrap (\_ -> ULDef, \_ -> undefined) $ xpElem "Default" $ xpUnit
             ]

data TP = TC | TB | TO | TS | TUS | TL | TUL | TLL | TULL | TF | TD | TT deriving (Show, Eq, Ord, Enum)
instance XmlPickler TP where
  xpickle = xpAlt tag ps
    where
      tag t = fromEnum t
      ps = [ xpWrap (\_ -> toEnum ix, \_ -> undefined) $ xpElem nm $ xpUnit
           | (ix,nm) <- zip [0..] names ]
      names = ["Char", "Boolean", "Octet", "Short", "UShort", "Long", "ULong"
              , "LongLong", "ULongLong", "Float", "Double", "Time" ]

data T = TP TP | TStr U.Bound | TSeq U.Bound T | TArr Int T | TR Name | TDef Def deriving (Show, Eq)
instance XmlPickler T where
  xpickle = xpAlt tag ps
    where
      tag (TP _)     = 0
      tag (TStr _)   = 1
      tag (TSeq _ _) = 2
      tag (TArr _ _) = 3
      tag (TR _)     = 4
      tag (TDef _)   = 5
      ps = [ xpWrap (TP, \(TP t) -> t) $ xpickle
           , xpWrap (TStr, \(TStr b) -> b) $
             xpElem "String" $ xpAttrImplied "length" xpickle
           , xpWrap (uncurry TSeq, \(TSeq b t) -> (b, t)) $
             xpElem "Sequence" $ xpPair (xpAttrImplied "size" xpickle) $ xpickle
           , xpWrap (uncurry TArr, \(TArr b t) -> (b, t)) $
             xpElem "Array" $ xpPair (xpAttr "size" xpickle) $ xpickle
           , xpWrap (TR, \(TR nm) -> nm) $
             (xpElem "Type" $ xpAttr "name" xpText)
           , xpWrap (TDef, \(TDef d) -> d) $
             xpickle
           ]

---------------------------------------------
---
---  Scopes from unpickled form
---
---------------------------------------------

data Scope = Scope { scParent    :: Maybe Scope,
                     scSubscopes :: [(Name, Scope)],
                     scDefs      :: [(Name, Def)] }

instance Show Scope where
  show (Scope ps xs ds) = "Scope " ++ (if isJust ps then "(Just _) " else "Nothing ")  ++ show xs ++ " " ++ show ds

cMD (MD ds) = res
  where
    res = Scope Nothing xs ys
    (Scope _ xs ys) = mergeScopes $ map (cDef (Just res)) ds

cDef :: Maybe Scope -> Def -> Scope
cDef ps (DM nm ds) = res
  where
    res = Scope ps [(nm, mergeScopes $ map (cDef (Just res)) ds)] []
cDef ps t@(DS nm ms) = res
  where
    res = Scope ps s' [(nm, t)]
    s = mergeScopes $ map (cM (Just res)) ms
    s' = if isEmptyScope s then [] else [(nm, s)]
    cM ps' (M _ t) = cT ps' t
cDef ps t@(DU nm dt cs) = res
  where
    res = Scope ps s' [(nm, t)]
    s = mergeScopes $ {-cT (Just res) dt :-} map (cUC (Just res)) cs -- discr type need not be in scope
    s' = if isEmptyScope s then [] else [(nm, s)]
    cUC ps' (UC _ t _) = cT ps' t
cDef ps t@(DE nm es) = res
  where
    res = Scope ps [(nm, subres)] [(nm, t)]
    subres = Scope (Just res) [] (map cE es)
    cE (E n v) = (n, DC n v)
cDef ps t@(DT nm _) = Scope ps [] [(nm, t)]
cDef ps t@(DC nm _) = Scope ps [] [(nm, t)]
cDef ps (DK _ _) = Scope ps [] []
cDef ps t@(DP nm pfx) = Scope ps [] [(nm ++ "'Prefix", t)] -- a bit of a hack to use a 'Prefix suffix

cT :: Maybe Scope -> T -> Scope
cT ps (TDef d) = cDef ps d
cT ps _ = Scope ps [] []

mergeScopes :: [Scope] -> Scope
mergeScopes = foldl merge nullScope
  where
    merge (Scope _ s1 d1) (Scope p s2 d2) = Scope p (s1++s2) (d1++d2)

nullScope = Scope Nothing [] []
isEmptyScope (Scope _ [] []) = True
isEmptyScope _ = False

---------------------------------------------
---
---  Name lookup in scope
---
---------------------------------------------

rootScope :: Scope -> Scope
rootScope sc@(Scope Nothing _ _) = sc
rootScope (Scope (Just p) _ _) = rootScope p

lookupDef :: Name -> Scope -> (Scope, Maybe Def)
lookupDef n scope
  | not isabs = lookupDef1 cs scope
  | otherwise = lookupDef1 (tail cs) (rootScope scope)
  where
    cs = splitOn "::" n
    isabs = null $ head cs

lookupDef1 :: [Name] -> Scope -> (Scope, Maybe Def)
lookupDef1 ns scope
  | (isJust.snd) match = match
  | isJust parent = lookupDef1 ns $ fromJust parent
  | otherwise = (scope, Nothing)
  where
    (Scope parent _ _) = scope
    match = look (scope, Nothing) ns scope
    look def [] _ = def
    look _ [n] sc@(Scope _ _ ds) = (sc, lookup n ds)
    look _ (n:ns) sc@(Scope _ xs _) = case lookup n xs of
      Nothing -> (sc, Nothing)
      (Just x) -> look (x, Nothing) ns x
