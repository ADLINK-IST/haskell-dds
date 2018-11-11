{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module DDS.Type (
  Signedness(..), Bound,
  UnionCaseLabel, UnionCase,
  IntWidth(..), FloatWidth(..),
  Type(..), TopicType(..)
  ) where

import Data.Text
import Data.Vector
import Data.HashMap.Strict

data Signedness = Signed | Unsigned deriving (Show, Eq)
type Bound = Maybe Int
type UnionCaseLabel = Maybe Integer
type UnionCase = ([UnionCaseLabel], Text, Type)
data IntWidth = I1 | I2 | I4 | I8 deriving (Show, Eq, Ord, Enum)
data FloatWidth = F4 | F8 deriving (Show, Eq, Ord, Enum)

data Type =
  TInt IntWidth Signedness
  | TFloat FloatWidth
  | TChar
  | TBool
  | TTime
  | TString Bound
  | TSequence Bound Type
  | TArray Int Type
  | TStruct [(Text, Type)] (HashMap Text Type)
  | TEnum (Vector Text) (HashMap Text Int)
  | TUnion Type Int [UnionCase] (HashMap Integer (Text,Type)) (Maybe (Text,Type))
  deriving (Show, Eq)

data TopicType = TopicType { topicTypeName :: String,
                             topicTypeSizeof :: Int,
                             topicTypeAlignof :: Int,
                             topicTypeType :: Type,
                             topicTypeKeylist :: [Text]
                           } deriving (Show, Eq)
