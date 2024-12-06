module TestExe71 where

import Maude
import Types (FeatureID(..), GroupID, FeatureType(..), GroupType(..), Name, FeatureModel)
import Types (AddOperation(..), ChangeOperation(..), UpdateOperation(..), TimePoint(..))
import Types hiding (Group, Feature, _childFeatures, _childGroups, _name, _groupID)
import qualified Data.Map as M

-- the initial feature model
createFM :: FM
createFM =
  let 

      rootFeatureID = FeatureID "fid 1"
      rootGroupID = GroupID "gid 1"

      -- Define IDs for childrenF
      childFeatureIDs = [FeatureID $ "fid " ++ show n | n <- [2..11]]
      child4SubIDs = [FeatureID $ "fid " ++ show n | n <- [12..14]]
      child10SubIDs = [FeatureID $ "fid " ++ show n | n <- [15..16]]

      -- Empty feature model containing the root feature only
      fmWithRoot = FM rootFeatureID $ M.singleton rootFeatureID
        (F { _name = "Root"
           , _parentID = rootFeatureID
           , _featureType = Mandatory
           , _childGroups = [Group { _groupID = rootGroupID, _groupType = Or, _childFeatures = [] }]
           })

      -- Add the 10 child features to the root
      fmWithChildren = foldl (\fm (fid, n) -> 
                              addFeature fm (fid, "Child" ++ show n, rootGroupID, Optional)) 
                             fmWithRoot 
                             (zip childFeatureIDs [2..11])

      -- Add a group and subchildren for Child 4
      fmWithChild4Sub = foldl (\fm (fid, n) -> 
                               addFeature fm (fid, "Child4_Sub" ++ show n, GroupID "gid_4", Optional)) 
                              (addGroup fmWithChildren (FeatureID "fid 4", GroupID "gid_4", Or)) 
                              (zip child4SubIDs [1..3])


      fmWithChild10Sub = foldl (\fm (fid, n) -> 
                                addFeature fm (fid, "Child10_Sub" ++ show n, GroupID "gid_10", Optional)) 
                               (addGroup fmWithChild4Sub (FeatureID "fid 10", GroupID "gid_10", Or)) 
                               (zip child10SubIDs [1..2])

  in fmWithChild10Sub 
  -- Final feature model 

-- Test scenario: Modify the feature model by applying a series of operations
test_exe71 :: FM
test_exe71 =
  let 
      -- Step 1: Remove two features
      featureIDsToRemove = [FeatureID "fid 11", FeatureID "fid 16"]
      removalOperations = [ChangeOperation (TP 0) (RemoveFeature fid) | fid <- featureIDsToRemove]

      -- Step 2: Add a group + 3  features to fid6 
      addGroup6Operation = AddOperation (Validity (TP 0) Forever) 
                             (AddGroup (GroupID "gid_6") Or (FeatureID "fid 6"))

      newFeaturesFor6 = [("Child6_Sub" ++ show n, FeatureID $ "fid " ++ show fid) | (n, fid) <- zip [1..3] [17..19]]
      addFeaturesTo6 = [AddOperation (Validity (TP 0) Forever) 
                           (AddFeature fid name Optional (GroupID "gid_6")) 
                       | (name, fid) <- newFeaturesFor6]

      -- Step 3: Add a group and two features to fid 12
      addGroup12Operation = AddOperation (Validity (TP 0) Forever) 
                              (AddGroup (GroupID "gid_12") Or (FeatureID "fid 12"))

      newFeaturesFor12 = [("Child12_Sub" ++ show n, FeatureID $ "fid " ++ show fid) | (n, fid) <- zip [1..2] [20..21]]
      addFeaturesTo12 = [AddOperation (Validity (TP 0) Forever) 
                            (AddFeature fid name Optional (GroupID "gid_12")) 
                        | (name, fid) <- newFeaturesFor12]

      -- Step 4: Remove specific features in FM
      additionalRemovals = 
        [ ChangeOperation (TP 0) (RemoveFeature (FeatureID "fid 18")) -- Remove Child6_Sub2
        , ChangeOperation (TP 0) (RemoveFeature (FeatureID "fid 21")) -- Remove Child12_Sub2
        ]

      -- Step 5: Re-add Child6_Sub2 to the same group
      reAddChild6_Sub2 = AddOperation (Validity (TP 0) Forever) 
                           (AddFeature (FeatureID "fid 18") "Child6_Sub2" Optional (GroupID "gid_6"))

      -- All operations
      operations = removalOperations ++ [addGroup6Operation] ++ addFeaturesTo6 ++ 
                   [addGroup12Operation] ++ addFeaturesTo12 ++ additionalRemovals ++ [reAddChild6_Sub2]

  in foldl (\fm op -> mkOp op fm) createFM operations

