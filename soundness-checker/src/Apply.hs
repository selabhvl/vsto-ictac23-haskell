module Apply (apply) where

import Control.Lens
import qualified Data.IntervalMap.Generic.Strict as IM
import Helpers
import Types

apply :: UpdateOperation -> IntervalBasedFeatureModel -> IntervalBasedFeatureModel
apply (AddOperation validity (AddFeature fid name fType parentID)) =
  insertName name validity fid
    . over
      (groupValidities . ix parentID . childValidities)
      (insertSingleton validity fid)
    . over
      (featureValidities . ix fid)
      ( \feature ->
          feature
            & existenceValidities %~ insert validity ()
            & nameValidities %~ insert validity name
            & typeValidities %~ insert validity fType
            & parentValidities %~ insert validity parentID
      )
    . insertEmptyFeature
    
     fid
apply (AddOperation validity (AddGroup gid gType parentID)) =
  over
    (featureValidities . ix parentID . childValidities)
    (insertSingleton validity gid)
    . over
      (groupValidities . ix gid)
      ( \group ->
          group
            & existenceValidities %~ insert validity ()
            & typeValidities %~ insert validity gType
            & parentValidities %~ insert validity parentID
      )
    . insertEmptyGroup gid
apply (ChangeOperation tp (RemoveFeature fid)) = \ibfm ->
  let (FeatureValidity fe fn ft fp fc) = ibfm ^?! featureValidities . ix fid
      Just (_, name) = lookupTP tp fn
      Just (_, parentID) = lookupTP tp fp
   in ibfm
        & nameValidities . ix name %~ clampIntervalEnd tp
        & featureValidities . ix fid
        .~ FeatureValidity
          (clampIntervalEnd tp fe)
          (clampIntervalEnd tp fn)
          (clampIntervalEnd tp ft)
          (clampIntervalEnd tp fp)
          fc
        & groupValidities . ix parentID . childValidities %~ clampIntervalEndValue tp fid
        & deleteFeatureIfNull fid
        & deleteNameIfNull name
apply (ChangeOperation tp (RemoveGroup gid)) = \ibfm ->
  let (GroupValidity ge gt gp gc) = ibfm ^?! groupValidities . ix gid
      Just (_, parentID) = lookupTP tp gp
   in ibfm
        & groupValidities . ix gid
        .~ GroupValidity
          (clampIntervalEnd tp ge)
          (clampIntervalEnd tp gt)
          (clampIntervalEnd tp gp)
          gc
          & featureValidities . ix parentID . childValidities %~ clampIntervalEndValue tp gid
          & deleteGroupIfNull gid
apply (ChangeOperation tp (MoveFeature fid newParent)) = \ibfm ->
  let Just (Validity _ e, oldParent) = ibfm ^?! featureValidities . ix fid . parentValidities . to (lookupTP tp)
   in ibfm
        & groupValidities . ix oldParent . childValidities
        %~ clampIntervalEndValue tp fid
        & groupValidities . ix newParent . childValidities
        %~ insertSingleton (Validity tp e) fid
        & featureValidities . ix fid . parentValidities
        %~ insert (Validity tp e) newParent . clampIntervalEnd tp
apply (ChangeOperation tp (MoveGroup gid newParent)) = \ibfm ->
  let Just (Validity _ e, oldParent) = ibfm ^?! groupValidities . ix gid . parentValidities . to (lookupTP tp)
   in ibfm
        & featureValidities . ix oldParent . childValidities
        %~ clampIntervalEndValue tp gid
        & featureValidities . ix newParent . childValidities
        %~ insertSingleton (Validity tp e) gid
        & groupValidities . ix gid . parentValidities
        %~ insert (Validity tp e) newParent . clampIntervalEnd tp
apply (ChangeOperation tp (ChangeFeatureType fid newType)) = \ibfm ->
  let types = ibfm ^?! featureValidities . ix fid . typeValidities
      Just (containingKey@(Validity s e), oldType) = lookupTP tp types
   in ibfm
        & featureValidities . ix fid . typeValidities
        %~ insert (Validity tp e) newType -- clampInterval in paper, but OK.
          . insert (Validity s tp) oldType
          . IM.delete containingKey
apply (ChangeOperation tp (ChangeGroupType gid newType)) = \ibfm ->
  let types = ibfm ^?! groupValidities . ix gid . typeValidities
      Just (containingKey@(Validity s e), oldType) = lookupTP tp types
   in ibfm
        & groupValidities . ix gid . typeValidities
        %~ insert (Validity tp e) newType -- ditto
          . insert (Validity s tp) oldType
          . IM.delete containingKey
apply (ChangeOperation tp (ChangeFeatureName fid newName)) = \ibfm ->
  let Just (Validity _ e, oldName) = ibfm ^?! featureValidities . ix fid . nameValidities . to (lookupTP tp)
   in ibfm
        & nameValidities . ix oldName
          %~ clampIntervalEnd tp
        & deleteNameIfNull oldName
        & id
          %~ insertName newName (Validity tp e) fid
        & featureValidities . ix fid . nameValidities
          %~ insert (Validity tp e) newName . clampIntervalEnd tp
