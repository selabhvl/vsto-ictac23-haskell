{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Maude where

import Control.Lens
import Data.Algebra.Boolean ((-->))
import qualified Data.Map as M
import Data.Maybe
import Types (FeatureID(..), GroupID, FeatureType(..), GroupType(..), Name)
import Types (AddOperation(..), ChangeOperation(..), UpdateOperation(..), TimePoint(..))

data Feature = F
  { _name :: Name
  , _parentID :: FeatureID
  , _featureType :: FeatureType
  , _childGroups :: [Group]
  }
  deriving (Show, Eq)

data Group = Group
  { _groupID :: GroupID
  , _groupType :: GroupType
  , _childFeatures :: [FeatureID]
  }
  deriving (Show, Eq)

type FT = M.Map FeatureID Feature
data FM = FM FeatureID FT
  deriving (Show, Eq)

makeFieldsNoPrefix ''Feature
makeFieldsNoPrefix ''Group

isWellFormed :: FT -> FeatureID -> Bool
isWellFormed ft fid = all isWellFormed' (M.assocs ft)
  where
     isWellFormed' (k,f) = fid == k --> not ( (_featureType f) == Mandatory && all (((==) And) . _groupType) (gs f))
     gs f = maybe [] (\pf -> filter (\g -> fid `elem` (_childFeatures g)) $ _childGroups pf) $ M.lookup (_parentID f) ft

-- Rule addFeature
addFeature :: FM -> (FeatureID, Name, GroupID, FeatureType) -> FM
addFeature (FM rfid ft) (newFid, newName, targetGroup, fType)
 | notExists newFid ft && isUniqueName newName ft && isWellFormed ft'' newFid = FM rfid ft''
 | otherwise   = error "Not allowed"
 where
   ft' = addFeatureToGroup ft targetGroup newFid
   parentFid = parentOfGroup ft targetGroup
   ft'' = M.insert newFid (F {_parentID = parentFid, _name = newName, _featureType = fType, _childGroups=[] }) ft'

notExists :: FeatureID -> FT -> Bool
notExists fid ft = M.notMember fid ft

mkGroupMap :: [Group] -> M.Map GroupID Group
mkGroupMap = M.fromList . (map (\x -> (_groupID x,x)))

isUniqueName :: Name -> FT -> Bool
isUniqueName n ft = all (\f -> _name f /= n) (M.elems ft)

addFeatureToGroup :: FT -> GroupID -> FeatureID -> FT
addFeatureToGroup ft targetGroup newFid = M.adjust (over childGroups (map (\g -> if targetGroup == _groupID g then over (childFeatures) (newFid :) g else g))) parentID ft
  where
    parentID = parentOfGroup ft targetGroup

parentOfGroup :: FT -> GroupID -> FeatureID
parentOfGroup ft gid = fromJust $ M.foldrWithKey (\k f acc -> if gid `elem` map _groupID (_childGroups f) then Just k else acc) Nothing ft

-- Rule removeFeature
removeFeature :: FM -> FeatureID -> FM
removeFeature (FM rfid ft) fid
  | rfid /= fid && null (_childGroups f) && gid = FM rfid ft''  -- force eval of `gid`
  | otherwise = error $ "removeFeature: " ++ show fid
  where
    f = fromJust $ M.lookup fid ft
    parentFid = _parentID f
    gid = parentGroup ft fid
    ft'' = removeFeatureFromParent (M.delete fid ft) parentFid fid

parentGroup :: FT -> FeatureID -> Bool -- not actually a "getter" in Maude, just error checking
parentGroup ft fid = not . null $ M.foldr (\f acc -> acc ++ filter (\g -> fid `elem` (_childFeatures g)) (_childGroups f) ) [] ft

removeFeatureFromParent :: FT -> FeatureID -> FeatureID -> FT
removeFeatureFromParent ft parentFid fid = M.adjust (over childGroups (\gs -> map (over childFeatures (filter (\f ->f /= fid))) gs)) parentFid ft

-- Rule moveFeature
moveFeature :: FM -> (FeatureID, GroupID) -> FM
moveFeature (FM rfid ft) (fid, newGroup)
  | rfid /= fid && not (isSubFeature fid newParent rfid ft) && gid = FM rfid ft''''
  | otherwise = error "moveFeature"
  where
    f = fromJust $ M.lookup fid ft
    parentFid = _parentID f
    ft' = M.delete fid ft
    newParent = parentOfGroup ft newGroup
    ft'' = removeFeatureFromParent ft' parentFid fid
    ft''' = addFeatureToGroup ft'' newGroup fid
    ft'''' = M.insert fid (over parentID (\_ -> newParent) f) ft''' -- could be M.adjust.
    gid = parentGroup ft fid

isSubFeature :: FeatureID -> FeatureID -> FeatureID -> FT -> Bool
isSubFeature fid parentFid rfid ft
  | parentFid == rfid = False
  | fid == parentFid = fid /= rfid -- 2 in 1
  | otherwise = isSubFeature fid (_parentID f) rfid ft -- careful!
  where
    f = fromJust $ M.lookup parentFid ft

-- Rule renameFeature
renameFeature :: FM -> (FeatureID, Name) -> FM
renameFeature (FM rfid ft) (fid, newName)
  | isUniqueName newName ft = FM rfid $ M.adjust (over name (const newName)) fid ft -- minor departure from Maude
  | otherwise = error "renameFeature"

-- Rule changeFeatureVariationType
changeFeatureVariationType :: FM -> (FeatureID, FeatureType) -> FM
changeFeatureVariationType (FM rfid ft) (fid, ftype')
  | fid /= rfid && isWellFormed ft'' fid = FM rfid ft''
  | otherwise = error "cfvt"
  where
    ft'' = M.adjust (over featureType (const ftype')) fid ft


-- Rule addGroup
addGroup :: FM -> (FeatureID, GroupID, GroupType) -> FM
addGroup (FM rfid ft) (fid, gid, gtype)
  | isJust fj && isUniqueGroupID gid ft -- Maude seems fishy here, reduntant update of FT?
    = FM rfit $ M.adjust (over childGroups ((Group {_groupID = gid, _groupType = gtype, _childFeatures= []}):)) fid ft
  where
    fj = M.lookup fid ft
    
-- Rule removeGroup
-- Rule changeGroupVariationType
-- Rule moveGroup

----- Some Tests
-- Try:
-- $ stack repl
-- ghci> test_exe1
-- ...

test_fm1 :: FM
test_fm1 = FM me $ M.singleton me $ F { _name = "Test1", _parentID = me, _featureType = Mandatory, _childGroups = []}
  where
    me = FeatureID "fid 1"

-- We're reusing the operations from Ida's code here, but of course TPs and intervals are ignored.
test_plan1 :: [UpdateOperation]
test_plan1 = [ChangeOperation (TP 0) (RemoveFeature (FeatureID "fid 1"))]

mkOp :: UpdateOperation -> (FM -> FM)
mkOp (ChangeOperation (TP 0) (RemoveFeature fid)) = \m -> removeFeature m fid
mkOp (AddOperation _ (AddFeature fid name fType gid)) = \m -> addFeature m (fid,name,gid,fType)
mkOp _ = error "Op NYI or TP-error (must be 0)!"

test_exe1 = foldl (\m op -> mkOp op $ m) test_fm1 test_plan1
