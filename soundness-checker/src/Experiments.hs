{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Experiments where


import Control.DeepSeq
import System.CPUTime
import Text.Printf

import qualified Data.Map as M
import Types (FeatureID(..), GroupID(..), FeatureType(..), GroupType(..), Name, FeatureModel)
import Types (AddOperation(..), ChangeOperation(..), UpdateOperation(..), TimePoint(..), Validity(..))
import Maude (FM(..), Feature(..), Group(..), Feature(F), _name, _parentID, _featureType, _childGroups, mkOp, prop_wf)

import qualified Apply
import qualified ExampleIntervalBasedFeatureModel

test_fm1 :: FM
test_fm1 = FM me $ M.singleton me $ F { _name = "Test1", _parentID = Nothing, _featureType = Mandatory, _childGroups = mempty}
  where
    me = FeatureID "fid 1"

fold_and_test :: FM -> [UpdateOperation] -> (Int, FM)
fold_and_test im = foldl (\(i,m) op -> let step = mkOp m op in if prop_wf False step then (i+1, step) 
                                                                  else error ("Op " ++ (show i) ++ "/" ++ show op ++ " produced a broken model.\n"++ show (prop_wf True step))) (1, im)


flatPlan :: FeatureID -> [UpdateOperation]
flatPlan rfid =
  let totalFeatures = 5000  
      readdFeatures = 1000  
      groupID = GroupID "gid_root"  
  in
  -- Add a single group under the root feature
  [ AddOperation (Validity (TP 0) Forever) (AddGroup groupID Or rfid) ] ++

  -- All features to the single group
  [ AddOperation (Validity (TP 0) Forever) 
      (AddFeature (FeatureID $ "fid_" ++ show i) 
                  ("Feature" ++ show i) 
                  Optional 
                  groupID)
  | i <- [1..totalFeatures] ] ++

  -- Remove every 10th feature
  [ ChangeOperation (TP 0) (RemoveFeature (FeatureID $ "fid_" ++ show i))
  | i <- [10,20..totalFeatures] ] ++

  --  Add a secondary group for moved features
  [ AddOperation (Validity (TP 0) Forever) (AddGroup (GroupID "gid_moved") Or rfid) ] ++

  -- Step 4: Move every 5th feature to a secondary group
  [ ChangeOperation (TP 0) (MoveFeature (FeatureID $ "fid_" ++ show i) (GroupID "gid_moved"))
  | i <- [5,15..totalFeatures], i `mod` 10 /= 0 ] ++ -- Skip already removed features

  -- ename every 7th feature
  [ ChangeOperation (TP 0) (ChangeFeatureName (FeatureID $ "fid_" ++ show i) 
                                             ("RenamedFeature" ++ show i))
  | i <- [7,14..totalFeatures], i `mod` 10 /= 0 ] ++ -- Skip already removed features

  -- Add back removed features to the original group
  [ AddOperation (Validity (TP 0) Forever) 
      (AddFeature (FeatureID $ "fid_readd_" ++ show i) 
                  ("ReaddedFeature" ++ show i) 
                  Optional 
                  groupID)
  | i <- [1..readdFeatures] ] ++

  -- Rename some of the re-added features
  [ ChangeOperation (TP 0) (ChangeFeatureName (FeatureID $ "fid_readd_" ++ show i) 
                                             ("RenamedReaddedFeature" ++ show i))
  | i <- [1,3..readdFeatures] ] ++  -- Rename every 2nd re-added feature

  -- Change the type of features
  [ ChangeOperation (TP 0) (ChangeFeatureType (FeatureID $ "fid_" ++ show i) Mandatory)
  | i <- [4,8..totalFeatures], i `mod` 10 /= 0 ] ++  
  [ ChangeOperation (TP 0) (ChangeFeatureType (FeatureID $ "fid_readd_" ++ show i) Optional)
  | i <- [2,4..readdFeatures] ] 


shallowHierarchyPlan :: FeatureID -> [UpdateOperation]
shallowHierarchyPlan rfid =
  let totalFeatures = 6000 
      totalGroups = 20      
      removedFeatures = 500 
      movedFeatures = 200   
      renamedFeatures = 300 
      changedTypes = 300    
  in

  [ AddOperation (Validity (TP 0) Forever) (AddGroup (GroupID $ "gid_" ++ show g) Or rfid)
  | g <- [1..totalGroups] ] ++

  [ AddOperation (Validity (TP 0) Forever) 
      (AddFeature (FeatureID $ "fid_" ++ show i) 
                  ("Feature" ++ show i) 
                  Optional 
                  (GroupID $ "gid_" ++ show (1 + (i `mod` totalGroups))))
  | i <- [1..totalFeatures] ] ++

  [ ChangeOperation (TP 0) (RemoveFeature (FeatureID $ "fid_" ++ show i))
  | i <- [10, 10 + totalGroups .. min totalFeatures (10 + 10 * removedFeatures)] ] ++

  [ ChangeOperation (TP 0) (MoveFeature (FeatureID $ "fid_" ++ show i) (GroupID $ "gid_" ++ show ((i `mod` totalGroups) + 1)))
  | i <- [5, 5 + totalGroups .. (5 + 5 * movedFeatures)], i <= totalFeatures, i `mod` 10 /= 0 ] ++ -- Skip removed features

  [ ChangeOperation (TP 0) (ChangeFeatureName (FeatureID $ "fid_" ++ show i) 
                                             ("RenamedFeature" ++ show i))
  | i <- [7, 7 + totalGroups .. (7 + 7 * renamedFeatures)], i <= totalFeatures, i `mod` 10 /= 0 ] ++ -- Skip removed features

  [ ChangeOperation (TP 0) (ChangeFeatureType (FeatureID $ "fid_" ++ show i) Mandatory)
  | i <- [4, 4 + totalGroups .. (4 + 4 * changedTypes)], i <= totalFeatures, i `mod` 10 /= 0 ] -- Skip removed features

hierarchyPlan :: FeatureID -> [UpdateOperation]
hierarchyPlan rfid =
  let
    rootGroupID = GroupID "gid_root"

    firstLevelFeatures = [FeatureID $ "fid_" ++ show n | n <- [2..11]]

    subFeatureIDs :: FeatureID -> [FeatureID]
    subFeatureIDs parentID =
      let featureIndex = last (words (show parentID)) -- Extract number from parentID
          base = read featureIndex * 10 -- Unique base ID for each parent
      in [FeatureID $ "fid_" ++ show (base + n) | n <- [1..2]]

    rootOperations =
      [ AddOperation (Validity (TP 0) Forever) (AddGroup rootGroupID Or rfid),
        AddOperation (Validity (TP 0) Forever) (AddFeature rfid "Root" Mandatory rootGroupID)
      ]

    -- Add first-level features
    firstLevelOperations =
      [ AddOperation (Validity (TP 0) Forever)
          (AddFeature fid ("Child_" ++ show n) Optional rootGroupID)
      | (fid, n) <- zip firstLevelFeatures [2..11]
      ]

    -- Add groups and sub-features for each first-level feature
    subFeatureOperations =
      concat
        [ let groupID = GroupID $ "gid_" ++ show n
          in
            AddOperation (Validity (TP 0) Forever)
              (AddGroup groupID Or parentID)
            :
            [ AddOperation (Validity (TP 0) Forever)
                (AddFeature subFid ("Sub_" ++ show n ++ "_" ++ show subIdx) Optional groupID)
            | (subFid, subIdx) <- zip (subFeatureIDs parentID) [1..]
            ]
        | (parentID, n) <- zip firstLevelFeatures [2..11]
        ]

  in
    rootOperations ++ firstLevelOperations ++ subFeatureOperations




balancedPlan1 :: FeatureID -> [UpdateOperation]
balancedPlan1 rfid =
  let 
      -- Reduced counts for root level groups features
      totalRootGroups = 20   
      totalRootFeatures = 500  
      subGroupsPerFeature = 4  
      featuresPerSubGroup = 5  

      readdFeatures = 200       
      movedFeatures = 300       
      renamedFeatures = 150    
      changedTypes = 200        

      makeFeatureID i = FeatureID $ "fid_" ++ show i
      makeGroupID i = GroupID $ "gid_" ++ show i

      rootGroupOperations =
        [ AddOperation (Validity (TP 0) Forever) (AddGroup (makeGroupID g) Or rfid)
        | g <- [1..totalRootGroups] ]

      -- features distributed across root groups
      rootFeatureOperations =
        [ AddOperation (Validity (TP 0) Forever) 
            (AddFeature (makeFeatureID i) 
                        ("Feature" ++ show i) 
                        Optional 
                        (makeGroupID ((i `mod` totalRootGroups) + 1)))
        | i <- [1..totalRootFeatures] ]

      -- Add sub-groups for each root level feature
      subGroupOperations =
        concat
          [ let parentFeature = makeFeatureID i
            in [ AddOperation (Validity (TP 0) Forever) 
                  (AddGroup (makeGroupID (i * 100 + g)) Or parentFeature)
               | g <- [1..subGroupsPerFeature] ]
          | i <- [1..totalRootFeatures] ]

      -- Add features to each subgroup
      subGroupFeatureOperations =
        concat
          [ let parentGroup = makeGroupID (i * 100 + g)
            in [ AddOperation (Validity (TP 0) Forever)
                  (AddFeature (makeFeatureID (i * 1000 + g * 10 + f))
                              ("SubFeature" ++ show (i * 1000 + g * 10 + f))
                              Optional
                              parentGroup)
               | f <- [1..featuresPerSubGroup] ]
          | i <- [1..totalRootFeatures], g <- [1..subGroupsPerFeature] ]

      -- Remove leaf features
      removeOperations =
        take 500 [ ChangeOperation (TP 0) (RemoveFeature (makeFeatureID (i * 1000 + g * 10 + f)))
        | i <- [1..totalRootFeatures],  
          g <- [1..subGroupsPerFeature],  
          f <- [1..featuresPerSubGroup] 
        ]

      moveOperations =
        take 300 [ ChangeOperation (TP 0) (MoveFeature (makeFeatureID i) (makeGroupID ((i `mod` totalRootGroups) + 1)))
        | i <- [5, 15..(5 + movedFeatures)], i `mod` 10 /= 0 ] 

      renameOperations =
        take 150 [ ChangeOperation (TP 0) (ChangeFeatureName (makeFeatureID i) 
                                                 ("RenamedFeature" ++ show i))
        | i <- [7, 17..(7 + renamedFeatures)], i `mod` 10 /= 0 ] 

      changeTypeOperations =
        take 200 [ ChangeOperation (TP 0) (ChangeFeatureType (makeFeatureID i) Mandatory)
        | i <- [4, 14..(4 + changedTypes)], i `mod` 10 /= 0 ] -- Skip removed features

      readdOperations =
        take 200 [ AddOperation (Validity (TP 0) Forever) 
            (AddFeature (FeatureID $ "fid_readd_" ++ show i) 
                        ("ReaddedFeature" ++ show i) 
                        Optional 
                        (makeGroupID ((i `mod` totalRootGroups) + 1)))
        | i <- [1..readdFeatures] ]

  in
    rootGroupOperations ++
    rootFeatureOperations ++
    subGroupOperations ++
    subGroupFeatureOperations ++
    removeOperations ++
    moveOperations ++
    renameOperations ++
    changeTypeOperations ++
    readdOperations

linearHierarchyPlan :: FeatureID -> [UpdateOperation]
linearHierarchyPlan rfid =
  let 
      totalFeatures = 1500 
      movedFeatures = 1500 
      renamedFeatures = 1500 
      changedTypes = 1500 
      readdFeatures = 1500 

      makeFeatureID i = FeatureID $ "fid_" ++ show i
      makeGroupID i = GroupID $ "gid_" ++ show i

      addFeatureOperations =
        concat
          [ [ AddOperation (Validity (TP 0) Forever)
                (AddGroup (makeGroupID i) Or (if i == 1 then rfid else makeFeatureID (i - 1))), -- Add a group for this feature
              AddOperation (Validity (TP 0) Forever)
                (AddFeature (makeFeatureID i) 
                            ("Feature_" ++ show i) 
                            Optional 
                            (makeGroupID i)) 
            ]
          | i <- [1..totalFeatures] ]

      moveOperations =
        take movedFeatures
          [ ChangeOperation (TP 0) 
              (MoveFeature (makeFeatureID i) (makeGroupID ((i - 1) `max` 1))) -- Move to the previous group's group
          | i <- [2..totalFeatures - 1] ] 

      renameOperations =
        take renamedFeatures
          [ ChangeOperation (TP 0) 
              (ChangeFeatureName (makeFeatureID i) ("RenamedFeature_" ++ show i))
          | i <- [2, 4..totalFeatures] ] 

      changeTypeOperations =
        take changedTypes
          [ ChangeOperation (TP 0) 
              (ChangeFeatureType (makeFeatureID i) Mandatory)
          | i <- [3, 6..totalFeatures] ] 

      readdOperations =
        take readdFeatures
          [ AddOperation (Validity (TP 0) Forever)
              (AddFeature (FeatureID $ "fid_readd_" ++ show i) 
                          ("ReaddedFeature_" ++ show i) 
                          Optional 
                          (makeGroupID totalFeatures)) 
          | i <- [1..readdFeatures] ]

  in
    addFeatureOperations ++
    moveOperations ++
    renameOperations ++
    changeTypeOperations ++
    readdOperations



gridHierarchyPlan :: FeatureID -> [UpdateOperation]
gridHierarchyPlan rfid =
  let 
      totalGroups = 100         
      featuresPerGroup = 100    
      removedFeatures = 200    
      movedFeatures = 300     
      renamedFeatures = 200    
      changedTypes = 200     
      readdFeatures = 100     

      makeFeatureID g f = FeatureID $ "fid_" ++ show g ++ "_" ++ show f
      makeGroupID g = GroupID $ "gid_" ++ show g

      rootGroupOperations =
        [ AddOperation (Validity (TP 0) Forever)
            (AddGroup (makeGroupID g) Or rfid)
        | g <- [1..totalGroups] ]

      -- add features to each group
      groupFeatureOperations =
        concat
          [ [ AddOperation (Validity (TP 0) Forever)
                (AddFeature (makeFeatureID g f)
                            ("Feature_" ++ show g ++ "_" ++ show f)
                            Optional
                            (makeGroupID g))
            | f <- [1..featuresPerGroup] ]
          | g <- [1..totalGroups] ]

      removeOperations =
        take removedFeatures
          [ ChangeOperation (TP 0) (RemoveFeature (makeFeatureID g f))
          | g <- [1..totalGroups], 
            f <- [featuresPerGroup] 
          ]

      moveOperations =
        take movedFeatures
          [ ChangeOperation (TP 0) 
              (MoveFeature (makeFeatureID g f) (makeGroupID ((g `mod` totalGroups) + 1)))
          | g <- [1..totalGroups], 
            f <- [1..featuresPerGroup],
            f `mod` 10 /= 0 ] 

      renameOperations =
        take renamedFeatures
          [ ChangeOperation (TP 0) 
              (ChangeFeatureName (makeFeatureID g f) ("RenamedFeature_" ++ show g ++ "_" ++ show f))
          | g <- [1..totalGroups],
            f <- [1..featuresPerGroup],
            f `mod` 10 /= 0 ] 

      changeTypeOperations =
        take changedTypes
          [ ChangeOperation (TP 0) 
              (ChangeFeatureType (makeFeatureID g f) Mandatory)
          | g <- [1..totalGroups],
            f <- [1..featuresPerGroup],
            f `mod` 10 /= 0 ] 

      -- Re-add removed features (limited to `readdFeatures`)
      readdOperations =
        take readdFeatures
          [ AddOperation (Validity (TP 0) Forever)
              (AddFeature (FeatureID $ "fid_readd_" ++ show i) 
                          ("ReaddedFeature_" ++ show i) 
                          Optional 
                          (makeGroupID ((i `mod` totalGroups) + 1)))
          | i <- [1..readdFeatures] ]

  in
    rootGroupOperations ++
    groupFeatureOperations ++
    removeOperations ++
    moveOperations ++
    renameOperations ++
    changeTypeOperations ++
    readdOperations





balancedPlan :: FeatureID -> [UpdateOperation]
balancedPlan rfid =
  let totalFeatures = 6000  
      totalGroups = 100     
      readdFeatures = 500   
      removedFeatures = 700
      movedFeatures = 1000  
      renamedFeatures = 500 
      changedTypes = 800    
  in
  [ AddOperation (Validity (TP 0) Forever) (AddGroup (GroupID $ "gid_" ++ show g) Or rfid)
  | g <- [1..totalGroups] ] ++

  [ AddOperation (Validity (TP 0) Forever) 
      (AddFeature (FeatureID $ "fid_" ++ show i) 
                  ("Feature" ++ show i) 
                  Optional 
                  (GroupID $ "gid_" ++ show (1 + (i `mod` totalGroups))))
  | i <- [1..totalFeatures] ] ++

  [ ChangeOperation (TP 0) (RemoveFeature (FeatureID $ "fid_" ++ show i))
  | i <- [10, 10 + totalGroups .. min totalFeatures (10 + 10 * removedFeatures)] ] ++

  [ ChangeOperation (TP 0) (MoveFeature (FeatureID $ "fid_" ++ show i) (GroupID $ "gid_" ++ show ((i `mod` totalGroups) + 1)))
  | i <- [5, 5 + totalGroups .. (5 + 5 * movedFeatures)], i <= totalFeatures, i `mod` 10 /= 0 ] ++ -- Skip removed features

  [ ChangeOperation (TP 0) (ChangeFeatureName (FeatureID $ "fid_" ++ show i) 
                                             ("RenamedFeature" ++ show i))
  | i <- [7, 7 + totalGroups .. (7 + 7 * renamedFeatures)], i <= totalFeatures, i `mod` 10 /= 0 ] ++ -- Skip removed features

  [ ChangeOperation (TP 0) (ChangeFeatureType (FeatureID $ "fid_" ++ show i) Mandatory)
  | i <- [4, 4 + totalGroups .. (4 + 4 * changedTypes)], i <= totalFeatures, i `mod` 10 /= 0 ] ++ -- Skip removed features

  [ AddOperation (Validity (TP 0) Forever) 
      (AddFeature (FeatureID $ "fid_readd_" ++ show i) 
                  ("ReaddedFeature" ++ show i) 
                  Optional 
                  (GroupID $ "gid_" ++ show ((i `mod` totalGroups) + 1)))
  | i <- [1..readdFeatures] ]


-- measure :: FM -> [UpdateOperation] -> IO ()
measure createFM apply_op check_op ds_plan operations = do
  putStrLn $ "Number of operations: " ++ show (length operations)
  start <- getCPUTime
  
  let result = foldl apply_op createFM operations
  -- We deepSeq here, since we shouldn't rely on check_op forcing evaluation.
  -- TODO: if we rely on check_op to deepSeq, there'd be no need to do it here.
  --       Currently, it looks like the Maude-version spends 50/50 here, or 0/100 w/o deepSeq.
  -- TODO: deal with failing FMEP-plan here.
  if ds_plan then do
    rnf result `seq` return () -- Ensure full evaluation of the result
  else do
    return ()
  t_exe <- getCPUTime -- for future use
  let check_result = check_op result
  print check_result
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  let diff_plan = (fromIntegral (t_exe - start)) / (10^12)
  let diff_check = (fromIntegral (end - t_exe)) / (10^12)
  printf "Computation time: %0.6f sec (plan: %0.6f wf-check: %0.6f)\n" (diff :: Double) (diff_plan :: Double) (diff_check :: Double)
  -- This will allow us to collect multiple test-runs easily:
  return (diff_plan, diff_check, check_result)


mrlp_experiment :: IO ()
mrlp_experiment = do
  -- Use `False` in production since a) we need the time, and b) should only plug in plans for which we know the result.
  measure hm mkOp (prop_wf False) True tailPlan
  return ()
  where
    im@(FM rfid _) = test_fm1
    hm = foldl mkOp im headPlan
    (headPlan, tailPlan) = splitAt 3 (balancedPlan rfid)

mrlp_experiment_tcs :: IO ()
mrlp_experiment_tcs = do
  measure hm (flip Apply.apply) (const "explicit wf-check not needed") True tailPlan
  return ()
  where
    im = ExampleIntervalBasedFeatureModel.exampleIntervalBasedFeatureModel
    hm = foldl (flip Apply.apply) im headPlan
    (headPlan, tailPlan) = splitAt 3 (balancedPlan (FeatureID "feature:car"))

-- PLANS:

-- flatPlan 
-- shallowHierarchyPlan 
-- hierarchyPlan 
-- balancedPlan1 
-- linearHierarchyPlan 
-- gridHierarchyPlan 
-- balancedPlan
