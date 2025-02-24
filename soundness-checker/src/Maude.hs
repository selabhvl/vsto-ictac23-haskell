{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Maude where

import GHC.Generics (Generic)
import Control.DeepSeq

import Control.Lens
import Control.Monad (liftM)
import Data.Algebra.Boolean ((-->))
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Tuple.Utils
import Test.QuickCheck
import Types (FeatureID(..), GroupID(..), FeatureType(..), GroupType(..), Name, FeatureModel)
import Types (AddOperation(..), ChangeOperation(..), UpdateOperation(..), TimePoint(..))

import Helpers (noDupes, isExactlyOne, isGtOne)

data Feature = F
  { _name :: Name
  , _parentID :: Maybe FeatureID -- root has no parent
  , _featureType :: FeatureType
  , _childGroups :: [Group]
  }
  deriving (Show, Eq, Generic, NFData)

childGroupsToAscList :: Feature -> [Group]
childGroupsToAscList = sortOn _groupID . _childGroups

data Group = G
  { _groupID :: GroupID
  , _groupType :: GroupType
  , _childFeatures :: [FeatureID]
  }
  deriving (Show, Eq, Generic, NFData)

childFeaturesToAscList :: Group -> [FeatureID]
childFeaturesToAscList = sort . _childFeatures

type FT = M.Map FeatureID Feature
data FM = FM FeatureID FT
  deriving (Show, Eq, Generic, NFData)

makeFieldsNoPrefix ''Feature
makeFieldsNoPrefix ''Group

isWellFormed :: FT -> FeatureID -> Bool
isWellFormed ft fid = all isWellFormed' (M.assocs ft)
  where
     isWellFormed' (k,f) = fid == k --> not ( (_featureType f) == Mandatory && all (((==) And) . _groupType) (gs f))
     gs f = maybe [] (\pf -> filter (\g -> fid `elem` (_childFeatures g)) $ _childGroups pf) $ M.lookup (fromJust $ _parentID f) ft

-- Rule addFeature
addFeature :: FM -> (FeatureID, Name, GroupID, FeatureType) -> FM
addFeature (FM rfid ft) (newFid, newName, targetGroup, fType)
 | notExists newFid ft && isUniqueName newName ft && isWellFormed ft'' newFid = FM rfid ft''
 | otherwise = error $ "Not allowed: "
                        ++ "\n  FeatureID Exists: " ++ show (not $ notExists newFid ft)
                        ++ "\n  Name Unique: " ++ show (not $ isUniqueName newName ft)
                        ++ "\n  Well-formed: " ++ show (not $ isWellFormed ft'' newFid)
 where
   ft' = addFeatureToGroup ft targetGroup newFid
   parentFid = parentOfGroup ft targetGroup
   ft'' = M.insert newFid (F {_parentID = Just parentFid, _name = newName, _featureType = fType, _childGroups=[] }) ft'

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
parentOfGroup ft gid = fromJust $ M.foldrWithKey (\k f acc -> if gid `elem` map _groupID (_childGroups f) then Just k else acc) (error $ "no parent found for group " ++ show gid) ft

-- Rule removeFeature
removeFeature :: FM -> FeatureID -> FM
removeFeature (FM rfid ft) fid
  | rfid /= fid && null (_childGroups f) && gid = FM rfid ft''  -- force eval of `gid`
  | otherwise = error $ "removeFeature: " ++ show fid ++ " not allowed.\n"
                         ++ "Feature Exists: " ++ show (M.member fid ft) ++ "\n"
                         ++ "Feature Details: " ++ show (M.lookup fid ft)
  where
    f = maybe (error $ "removeFeature: " ++ show fid ++ " not found.") (id) $ M.lookup fid ft
    parentFid = maybe (error $ "removeFeature: parentID shouldn't be Nothing: " ++ show f ) (id) $ _parentID f
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
    parentFid = fromJust $ _parentID f
    ft' = M.delete fid ft
    newParent = parentOfGroup ft newGroup
    ft'' = removeFeatureFromParent ft' parentFid fid
    ft''' = addFeatureToGroup ft'' newGroup fid
    ft'''' = M.insert fid (over parentID (const (Just newParent)) f) ft''' -- could be M.adjust.
    gid = parentGroup ft fid

isSubFeature :: FeatureID -> FeatureID -> FeatureID -> FT -> Bool
isSubFeature fid parentFid rfid ft
  | parentFid == rfid = False
  | fid == parentFid = fid /= rfid -- 2 in 1
  | otherwise = isSubFeature fid (fromJust $ _parentID f) rfid ft -- careful!
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
    = FM rfid $ M.adjust (over childGroups ((G {_groupID = gid, _groupType = gtype, _childFeatures= []}):)) fid ft
  | otherwise = error "addGroup"
  where
    fj = M.lookup fid ft

isUniqueGroupID :: GroupID -> FT -> Bool
isUniqueGroupID gid ft = and . M.elems $ M.map (\f -> all (\g -> gid /= _groupID g) (_childGroups f)) ft

-- Rule removeGroup
removeGroup :: FM -> GroupID -> FM
removeGroup (FM rfid ft) gid
  | isJust fidj && (null . _childFeatures . snd3 . fromJust) fidj = FM rfid $ M.adjust (over childGroups (const gs)) fid ft
  | otherwise = error "removeGroup"
  where
     fidj = M.foldrWithKey (\fk f acc -> let (g,gs) = partition (\gx -> _groupID gx == gid) (_childGroups f) in if (not . null) g then Just (fk, head g, gs) else acc) Nothing ft
     fid  = fst3 . fromJust $ fidj
     gs   = thd3 . fromJust $ fidj

-- Rule changeGroupVariationType
changeGroupVariationType :: FM -> (GroupID, GroupType) -> FM
changeGroupVariationType (FM rfid ft) (gid, gtype)
  | isJust fidj && allWellFormed fs ft'' = FM rfid ft''
  | otherwise = error "cgvt"
  where
     fidj = M.foldrWithKey (\fk f acc -> let (g,gs) = partition (\gx -> _groupID gx == gid) (_childGroups f) in if (not . null) g then Just (fk, head g, gs) else acc) Nothing ft
     parentFid = fst3 . fromJust $ fidj
     gs   = thd3 . fromJust $ fidj
     fs   = _childFeatures . snd3 . fromJust $ fidj
     -- couldn't be bothered to do the 2nd `over`
     ft'' = M.adjust (over childGroups (const (G {_groupID=gid, _groupType=gtype, _childFeatures = _childFeatures . snd3 . fromJust $ fidj} : gs))) parentFid ft

allWellFormed :: [FeatureID] -> FT -> Bool
allWellFormed fids ft = all (isWellFormed ft) fids

-- Rule moveGroup
moveGroup :: FM -> (GroupID, FeatureID) -> FM
moveGroup (FM rfid ft) (gid, newParent)
  | isJust oldParentJ && allNotSubFeature fs newParent rfid ft = FM rfid ft''''
  | otherwise = error "moveGroup"
  where
    fnewj = M.lookup newParent ft
    f = fromJust fnewj
    ft' = M.delete newParent ft -- let's stick to Maude here
    oldParentJ = M.foldrWithKey (\fk f acc -> let (g,gs) = partition (\gx -> _groupID gx == gid) (_childGroups f) in if (not . null) g then Just ((fk,f), head g, gs) else acc) Nothing ft'
    oldParent = fst . fst3 . fromJust $ oldParentJ
    oldParentf = snd . fst3 . fromJust $ oldParentJ
    gs' = thd3 . fromJust $ oldParentJ
    g = snd3 . fromJust $ oldParentJ
    fs = _childFeatures g
    ft'' = M.delete oldParent ft'
    ft''' = M.insert newParent (over childGroups (g:) f) $ M.insert oldParent (over childGroups (const gs') oldParentf) ft''
    ft'''' = updateParents ft''' fs newParent

allNotSubFeature :: [FeatureID] -> FeatureID -> FeatureID -> FT -> Bool
allNotSubFeature fs fid rfid ft = all (\f -> not $ isSubFeature f fid rfid ft) fs

updateParents :: FT -> [FeatureID] -> FeatureID -> FT
-- lookup >>= adjust -> probably not efficient, but survivable
updateParents ft fs newParent = foldr (\fid acc -> maybe (error $ "Feature doesn't exist: " ++ show fid) (\_f-> M.adjust (over parentID (const (Just newParent))) fid acc) (M.lookup fid ft) ) ft fs

mkOp :: FM -> UpdateOperation -> FM
mkOp m (ChangeOperation (TP 0) (RemoveFeature fid)) = removeFeature m fid
mkOp m (ChangeOperation (TP 0) (RemoveGroup gid)) = removeGroup m gid
mkOp m (ChangeOperation (TP 0) (MoveFeature fid gid)) = moveFeature m (fid, gid)
mkOp m (ChangeOperation (TP 0) (MoveGroup gid fid)) = moveGroup m (gid, fid)
mkOp m (ChangeOperation (TP 0) (ChangeFeatureType fid fType)) = changeFeatureVariationType m (fid, fType)
mkOp m (ChangeOperation (TP 0) (ChangeGroupType gid gType)) = changeGroupVariationType m (gid, gType)
mkOp m (ChangeOperation (TP 0) (ChangeFeatureName fid name)) = renameFeature m (fid, name)
mkOp m (AddOperation _ (AddFeature fid name fType gid)) = addFeature m (fid,name,gid,fType)
mkOp m (AddOperation _ (AddGroup gid gType fid)) = addGroup m (fid,gid,gType)
mkOp _ _ = error "TP must be 0, we're not using it!"

featureModelToFM :: FeatureModel -> FM
featureModelToFM _ = error "TODO: Translation from Ida's iv-based plans to the flat one here not yet implemented!"

----- WF
-- Possibly missing:
-- + all GroupIDs are unique

wf1 :: FM -> Bool -- [VS] Checking a bit /harder/ here.
wf1 (FM rfid ft) = M.member rfid ft && (isNothing . _parentID . fromJust $ (M.lookup rfid ft))
                                    && and (map (\(fid,f) -> fid == rfid || (fromJust . _parentID $ f) `M.member` ft) (M.assocs ft))

wf2 :: FM -> Bool
wf2 (FM rfid ft) = (maybe False (\f -> _featureType f == Mandatory)) $ M.lookup rfid ft

wf3 :: FM -> Bool
wf3 (FM _rfid ft) = noDupes . (map _name) $ M.elems ft

wf4 :: FM -> Bool
wf4 (FM _rfid ft) = True -- TODO: NOP?

wf5 :: FM -> Bool
wf5 fm@(FM rfid ft) = all (isExactlyOne . filter (== True)) featSeen -- filter id?
  where
    ftNoRoot = M.delete rfid ft
    featSeen = map (\f -> map (\g -> f `elem` (_childFeatures g)) (allGroups fm)) $ M.keys ftNoRoot
 
wf6 :: [Group] -> FM -> Bool
wf6 ag fm@(FM rfid ft) = all (\g ->isExactlyOne . filter (\(gid,_) -> gid == _groupID g) $ gidMap) ag
  where
    gidMap = concatMap (\(fid,f) -> map (\g -> (_groupID g, fid)) (_childGroups f) ) $ M.assocs ft :: [(GroupID, FeatureID)]

wf7 :: [Group] -> FM -> Bool
wf7 ag fm@(FM rfid ft) = all (\g -> if _groupType g `elem` [Alternative, Or] then all (\fid -> (/= Mandatory) . _featureType . fromJust $ M.lookup fid ft) $ _childFeatures g else True) ag

wf8 :: [Group] -> FM -> Bool
-- TODO: eliminate length
wf8 ag fm@(FM rfid ft) = all (\g -> if _groupType g `elem` [Alternative, Or] then isGtOne (_childFeatures g) else True) ag

allGroups :: FM -> [Group]
allGroups (FM rfid ft) = concatMap _childGroups $ M.elems ft

----- QuickCheck
-- TODO: needs some fusion...
prop_wf :: Bool -> FM -> Bool
prop_wf shouldFail fm
  | and results = True
  | otherwise = if shouldFail then error $ "Some WF failed: " ++ show ((map fst) . (filter (not . snd)) . (zip [1..]) $ results) else False
  where
    ag = allGroups fm
    results = map (\f -> f fm) [wf1, wf2, wf3, wf4, wf5, wf6 ag, wf7 ag] -- XXX , wf8 allgroups]

-- Simple example. Should fail since wf3 is ofc more complex.
prop_wf21 :: FM -> Property
prop_wf21 fm = wf2 fm ==> wf3 fm

-- generate some reasonable names:
instance Arbitrary FeatureID where
  arbitrary = liftM FeatureID (("fid_" ++) . getASCIIString <$> resize 5 arbitrary)
instance Arbitrary GroupID where
  arbitrary = liftM GroupID (("gid_" ++) . getASCIIString <$> resize 5 arbitrary)

instance Arbitrary Feature where
  arbitrary = do
    name <- ("F_" ++) . getASCIIString <$> resize 5 arbitrary
    pid <- arbitrary -- TODO
    ftype <- oneof [return Mandatory, return Optional]
    cgs <- arbitrary -- TODO
    return $ F { _name = name, _parentID = pid, _featureType = ftype, _childGroups = cgs }

instance Arbitrary Group where
  arbitrary = do
    gid <- arbitrary
    t <- oneof [return Alternative, return Or, return And]
    cfs <- arbitrary
    return $ G {_groupID = gid, _groupType = t, _childFeatures = cfs}

instance Arbitrary FM where
  arbitrary = do
    rf <- arbitrary
    rfid <- liftM FeatureID arbitrary
    -- fix parent for root:
    return (FM rfid (M.singleton rfid (over parentID (const Nothing) rf)))

-- Experiments + Plans are now in Experimnts.hs

