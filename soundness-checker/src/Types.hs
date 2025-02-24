{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Control.Lens
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Map as M
import qualified Data.Set as S

newtype FeatureID = FeatureID String deriving (Show, Eq, Ord, Generic, NFData)
type RootID = FeatureID
newtype GroupID = GroupID String deriving (Show, Eq, Ord, Generic, NFData)
type Name = String

data GroupType
  = Alternative
  | Or
  | And
  deriving (Show, Eq, Ord, Generic, NFData)

data FeatureType
  = Optional
  | Mandatory
  deriving (Show, Eq, Ord, Generic, NFData)

--------------------
--   VALIDITIES   --
--------------------

type ValidityMap a = IM.IntervalMap Validity a

data TimePoint
  = TP Int
  | Forever
  deriving (Eq, Ord, Generic, NFData)

instance Show TimePoint where
  show Forever = "∞"
  show (TP i) = "(TP " ++ show i ++ ")"

data Validity = Validity
  { _start :: TimePoint
  , _end :: TimePoint
  }
  deriving (Show, Eq, Ord, Generic, NFData)

-- Intervals are half-closed [x, y) -- left-inclusive, right-exclusive
instance IM.Interval Validity TimePoint where
  lowerBound = _start
  upperBound = _end
  rightClosed _ = False

instance Semigroup Validity where
  Validity s1 e1 <> Validity s2 e2 = Validity (min s1 s2) (max e1 e2)

data IntervalBasedFeatureModel = IntervalBasedFeatureModel
  { _rootID :: RootID
  , _nameValidities :: NameValidities
  , _featureValidities :: FeatureValidities
  , _groupValidities :: GroupValidities
  }
  deriving (Show, Eq, Generic, NFData)

type NameValidities = M.Map Name (ValidityMap FeatureID)
type FeatureValidities = M.Map FeatureID FeatureValidity
type GroupValidities = M.Map GroupID GroupValidity

data FeatureValidity = FeatureValidity
  { _existenceValidities :: ValidityMap ()
  , _nameValidities :: ValidityMap Name
  , _typeValidities :: ValidityMap FeatureType
  , _parentValidities :: ValidityMap GroupID
  , _childValidities :: ValidityMap (S.Set GroupID)
  }
  deriving (Show, Eq, Generic, NFData)

data GroupValidity = GroupValidity
  { _existenceValidities :: ValidityMap ()
  , _typeValidities :: ValidityMap GroupType
  , _parentValidities :: ValidityMap FeatureID
  , _childValidities :: ValidityMap (S.Set FeatureID)
  }
  deriving (Show, Eq, Generic, NFData)

------------------------
--   FEATURE MODELS   --
------------------------

newtype FeatureModel = FeatureModel {_rootFeature :: Feature}
  deriving (Show, Eq, Generic, NFData)

type TreeSequence = [(TimePoint, FeatureModel)]

data EvolutionPlan = EvolutionPlan
  { _initialModel :: FeatureModel
  , _initialTime :: TimePoint
  , _operations :: [UpdateOperation]
  }
  deriving (Show, Generic, NFData)

data Feature = Feature
  { _featureID :: FeatureID
  , _name :: Name
  , _varType :: FeatureType
  , _childGroups :: [Group]
  }
  deriving (Show, Eq, Generic, NFData)

data Group = Group
  { _groupID :: GroupID
  , _varType :: GroupType
  , _childFeatures :: [Feature]
  }
  deriving (Show, Eq, Generic, NFData)

data UpdateOperation
  = AddOperation Validity AddOperation
  | ChangeOperation TimePoint ChangeOperation
  deriving (Show, Eq, Generic, NFData)

data AddOperation
  = AddFeature FeatureID Name FeatureType GroupID
  | AddGroup GroupID GroupType FeatureID
  deriving (Show, Eq, Generic, NFData)

data ChangeOperation
  = RemoveFeature FeatureID
  | RemoveGroup GroupID
  | MoveFeature FeatureID GroupID
  | MoveGroup GroupID FeatureID
  | ChangeFeatureType FeatureID FeatureType
  | ChangeGroupType GroupID GroupType
  | ChangeFeatureName FeatureID Name
  deriving (Show, Eq, Generic, NFData)

--------------------
-- Error Messages --
--------------------

data ValidationError
  = NodeAlreadyExists Validity
  | ParentNotExists
  | NameInUse Validity FeatureID
  | IncompatibleTypes FeatureType GroupType Validity
  | ChangePlanned TimePoint ChangeOperation
  | NodeNotExists
  | HasChildren Validity
  | CreatesCycle
  deriving (Show, Eq, Generic, NFData)

makePrisms ''GroupType
makePrisms ''TimePoint
makeFieldsNoPrefix ''Validity
makeFieldsNoPrefix ''IntervalBasedFeatureModel
makeFieldsNoPrefix ''FeatureValidity
makeFieldsNoPrefix ''GroupValidity
makePrisms ''FeatureType
makeFieldsNoPrefix ''Feature
makeFieldsNoPrefix ''Group
makeFieldsNoPrefix ''FeatureModel
makeFieldsNoPrefix ''EvolutionPlan
makePrisms ''UpdateOperation
