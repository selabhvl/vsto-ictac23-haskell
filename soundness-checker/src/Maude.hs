module Maude where

import Control.Lens
import Types

addFeature :: FeatureModel -> (FeatureID, Name, GroupID, FeatureType) -> FeatureModel
addFeature old (newFid, newName, targetGroup, fType)
 | notExists newFid (_rootFeature old) && isUniqueName newName (_rootFeature old) = FeatureModel { _rootFeature = addFeatureToGroup (_rootFeature old) targetGroup newFid }
 | otherwise   = error "Not allowed"

notExists :: FeatureID -> Feature -> Bool
notExists fid f
 | _featureID f == fid = False
 | otherwise = all (\g -> all (notExists fid) (_childFeatures g)) (_childGroups f)

isUniqueName :: Name -> Feature -> Bool
isUniqueName n f
 | _name f == n = False
 | otherwise = all (\g -> all (isUniqueName n) (_childFeatures g)) (_childGroups f)

addFeatureToGroup :: Feature -> GroupID -> FeatureID -> Feature
addFeatureToGroup f targetGroup newFid = updateF f (_childGroups) (map (addFeatureToGroup_g targetGroup newFid) (_childGroups f))

addFeatureToGroup_g :: GroupID -> FeatureID -> Group -> Group
addFeatureToGroup_g gid fid g
 | (_groupID g) == gid = over (childFeatures) (\ fs -> (Feature { _featureID = fid}):fs) g -- incomplete
 
updateF f sel func = error "NYI"