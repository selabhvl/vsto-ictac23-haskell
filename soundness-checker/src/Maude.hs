{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Maude where

import Control.Lens
import qualified Data.Map as M
import Types

type FT = M.Map FeatureID Feature
data FM = FM FeatureID FT
  deriving (Show, Eq)

makeFieldsNoPrefix ''FM

addFeature :: FM -> (FeatureID, Name, GroupID, FeatureType) -> FM
addFeature (FM rfid ft) (newFid, newName, targetGroup, fType)
 | notExists newFid ft && isUniqueName newName (((map snd) . M.toList) ft) = FM rfid (addFeatureToGroup ft targetGroup newFid)
 | otherwise   = error "Not allowed"

notExists :: FeatureID -> FT -> Bool
notExists fid ft = M.notMember fid ft

mkGroupMap :: [Group] -> M.Map GroupID Group
mkGroupMap = M.fromList . (map (\x -> (_groupID x,x)))

mkFeatureMap :: [Feature] -> FT
mkFeatureMap = M.fromList . (map (\x -> (_featureID x,x)))

isUniqueName :: Name -> [Feature] -> Bool
isUniqueName n ft = all (\f -> all (\g -> isUniqueName n (_childFeatures g))
                                   (_childGroups f)) ft

addFeatureToGroup :: FT -> GroupID -> FeatureID -> FT
addFeatureToGroup ft targetGroup newFid = M.map (\f -> updateF f (_childGroups) (map (addFeatureToGroup_g targetGroup newFid) (_childGroups f))) ft

addFeatureToGroup_g :: GroupID -> FeatureID -> Group -> Group
addFeatureToGroup_g gid fid g
 | (_groupID g) == gid = over (childFeatures) (\ fs -> (Feature { _featureID = fid}):fs) g -- incomplete
 
updateF f sel func = error "NYI"