{-# LANGUAGE DuplicateRecordFields #-}

-- Authors: Volker Stolz, Charaf Eddine Dridi @ HVL

module Experiments where
import Control.DeepSeq
import Control.Monad (foldM, unless)
import Data.Either
import Data.Foldable (foldl')
import Data.Maybe
import Data.Tuple.Utils
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.Map as M
import System.CPUTime
import System.IO
import Text.Printf

import qualified Types as T (Feature(..), Group(..))
import Types (Feature, FeatureModel(..), FeatureID(..), Group, GroupID(..), FeatureType(..), GroupType(..), FeatureModel, IntervalBasedFeatureModel(..))
import Types (AddOperation(..), ChangeOperation(..), UpdateOperation(..), TimePoint(..), Validity(..), ValidityMap, FeatureValidity(..))
-- Import Maude/Maude2 here:
import Maude2 (FM(..), Feature(..), FT(..), Group(..), Feature(F), _name, _parentID, _featureType, _childGroups, mkOp, prop_wf, childFeaturesToAscList, childGroupsToAscList)

import qualified Apply
import Helpers
import Validate
import Program (validateAndApply)
import TreeSequence (treeAt)

import Test.HUnit
import Test.QuickCheck

import Criterion.Main
import Criterion.Types hiding (measure)

-- shared for all models:
root_feature :: FeatureID
root_feature = FeatureID "fid 1"

-- Maude:
test_fm1 :: FM
test_fm1 = FM root_feature $ M.singleton root_feature $ F { _name = "Test1", _parentID = Nothing, _featureType = Mandatory, _childGroups = mempty}

-- FMEP:
v :: TimePoint -> TimePoint -> Validity
v = Validity

(∞) :: TimePoint
(∞) = Forever

im :: [(Validity, a)] -> ValidityMap a
im = IM.fromList

test_ifm1 :: IntervalBasedFeatureModel
test_ifm1 = IntervalBasedFeatureModel root_feature (M.fromList [("Test1", im [(Validity (TP 0) (∞), root_feature)])])
                                                   (M.fromList [(root_feature, FeatureValidity
              (im [(v (TP 0) (∞), ())])
              (im [(v (TP 0) (∞), "Test1")])
              (im [(v (TP 0) (∞), Mandatory)])
              mempty
              (im [])
          )])
                                                   (M.fromList [])
singleAddGroupOp :: FM -> UpdateOperation
singleAddGroupOp im = AddOperation (Validity (TP 0) Forever) 
                        (AddGroup (GroupID "Test_Group") 
                                  Or root_feature)


fold_and_test :: FM -> [UpdateOperation] -> (Int, FM)
fold_and_test im = foldl (\(i,m) op -> let step = mkOp m op in if prop_wf False step then (i+1, step) 
                                                                  else error ("Op " ++ (show i) ++ "/" ++ show op ++ " produced a broken model.\n"++ show (prop_wf True step))) (1, im)

-- NB: the Maude-approach does not really deal with timepoints. All timepoints ("at N do op1, op1, ...") must be totally ordered.
-- Essentially, all operations are executed after a linearizitation. Inserting an update somewhere may thus require relabbeling of (integeger) timepoints to make room "in between".
-- We streamline between both approaches by considering all actions in a plan totally ordered, with an interval of [0,∞).
-- We can then reuse the datatypes here, where the Maude-version simply ignores the intervals.
 

flatPlan :: Int -> FeatureID -> [UpdateOperation]
flatPlan scaleFactor rfid =
  let
      totalFeatures = 5000 `div` scaleFactor
      readdFeatures = 1000 `div` scaleFactor
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

  -- ename every 7th feature
  [ ChangeOperation (TP 0) (ChangeFeatureName (FeatureID $ "fid_" ++ show i) 
                                             ("RenamedFeature" ++ show i))
  | i <- [7,14..totalFeatures], i `mod` 10 /= 0 ] ++
  -- Remove every 10th feature
  [ ChangeOperation (TP 0) (RemoveFeature (FeatureID $ "fid_" ++ show i))
  | i <- [10,20..totalFeatures] ] ++

  --  Add a secondary group for moved features
  [ AddOperation (Validity (TP 0) Forever) (AddGroup (GroupID "gid_moved") Or rfid) ] ++

  -- Step 4: Move every 5th feature to a secondary group
  [ ChangeOperation (TP 0) (MoveFeature (FeatureID $ "fid_" ++ show i) (GroupID "gid_moved"))
  | i <- [5,15..totalFeatures], i `mod` 10 /= 0 ] ++ -- Skip already removed features
 -- Skip already removed features

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
   -- Step 9: Change the type of features to Mandatory, but only in the AND group
  [ ChangeOperation (TP 0) (ChangeFeatureType (FeatureID $ "fid_" ++ show i) Mandatory)
  | i <- [4,8..totalFeatures],
    i `mod` 10 /= 0,                  -- Exclude removed features
    i `mod` 5 == 0 ] ++               -- Only include features moved to the AND group
  
  [ ChangeOperation (TP 0) (ChangeFeatureType (FeatureID $ "fid_readd_" ++ show i) Optional)
  | i <- [2,4..readdFeatures] ] 


shallowPlan :: Int -> FeatureID -> [UpdateOperation]
shallowPlan scaleFactor rfid =
  let 
    totalFeatures = 6000 `div` scaleFactor
    totalGroups = 20
    removedFeatures = 500 `div` scaleFactor
    movedFeatures = 200   `div` scaleFactor
    renamedFeatures = 300 `div` scaleFactor
    changedTypes = 300    `div` scaleFactor
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
  | i <- [4, 4 + totalGroups .. (4 + totalGroups * changedTypes)],
    i <= totalFeatures,
    i `mod` 10 /= 0,
    let groupIndex = (i `mod` totalGroups) + 1
    in groupIndex > totalGroups `div` 2 ]

hierarchyPlan :: Int -> FeatureID -> [UpdateOperation]
hierarchyPlan scaleFactor rfid =
  let
    rootGroupID = GroupID "gid_root"

    -- Scale the range of first-level features
    firstLevelFeatures = [FeatureID $ "fid_" ++ show n | n <- [2..(11 `div` scaleFactor)]]

    subFeatureIDs :: FeatureID -> [FeatureID]
    subFeatureIDs (FeatureID parentStr) =
      let base = case reads (drop 4 parentStr) :: [(Int, String)] of
                   [(n, _)] -> n * 10
                   _        -> error $ "Invalid FeatureID format: " ++ parentStr
      in [FeatureID $ "fid_" ++ show (base + n) | n <- [1..(2 `div` scaleFactor)]]

    rootOperations =
      [ AddOperation (Validity (TP 0) Forever) (AddGroup rootGroupID Or rfid) ]

    -- Add first-level features with scaling
    firstLevelOperations =
      [ AddOperation (Validity (TP 0) Forever)
          (AddFeature fid ("Child_" ++ show n) Optional rootGroupID)
      | (fid, n) <- zip firstLevelFeatures [2..(11 `div` scaleFactor)]
      ]

    -- Add groups and sub-features for each first-level feature with scaling
    subFeatureOperations =
      concat
        [ let groupID = GroupID $ "gid_" ++ show n
          in
            AddOperation (Validity (TP 0) Forever)
              (AddGroup groupID Or parentID)
            :
            [ AddOperation (Validity (TP 0) Forever)
                (AddFeature subFid ("Sub_" ++ show n ++ "_" ++ show subIdx) Optional groupID)
            | (subFid, subIdx) <- zip (subFeatureIDs parentID) [1..(2 `div` scaleFactor)]
            ]
        | (parentID, n) <- zip firstLevelFeatures [2..(11 `div` scaleFactor)]
        ]

  in
    rootOperations ++ firstLevelOperations ++ subFeatureOperations




balancedPlan :: Int -> FeatureID -> [UpdateOperation]
balancedPlan scaleFactor rfid =
  let
    totalRootGroups = 20 `div` scaleFactor
    totalRootFeatures = 500 `div` scaleFactor
    subGroupsPerFeature = 4 `div` scaleFactor
    featuresPerSubGroup = 5 `div` scaleFactor
    readdFeatures = 200 `div` scaleFactor
    movedFeatures = 300 `div` scaleFactor
    renamedFeatures = 150 `div` scaleFactor
    changedTypes = 200 `div` scaleFactor

    makeFeatureID i = FeatureID $ "fid_" ++ show i
    makeGroupID i = GroupID $ "gid_" ++ show i

    rootGroupOperations =
      [ AddOperation (Validity (TP 0) Forever)
          (AddGroup (makeGroupID g)
                    (if g <= totalRootGroups `div` 2 then Or else And)
                    rfid)
      | g <- [1..totalRootGroups] ]

    -- Features distributed across root groups
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
      take 500
        [ ChangeOperation (TP 0) (RemoveFeature (makeFeatureID (i * 1000 + g * 10 + f)))
        | i <- [1..totalRootFeatures],
          g <- [1..subGroupsPerFeature],
          f <- [1..featuresPerSubGroup]
        ]

    moveOperations =
      take 300
        [ ChangeOperation (TP 0) (MoveFeature (makeFeatureID i) (makeGroupID ((i `mod` totalRootGroups) + 1)))
        | i <- [5, 15..(5 + movedFeatures)], i `mod` 10 /= 0 ]

    renameOperations =
      take 150
        [ ChangeOperation (TP 0) (ChangeFeatureName (makeFeatureID i)
                                                    ("RenamedFeature" ++ show i))
        | i <- [7, 17..(7 + renamedFeatures)], i `mod` 10 /= 0 ]

    changeTypeOperations =
      [ ChangeOperation (TP 0) (ChangeFeatureType (makeFeatureID i) Optional)
      | i <- [1..totalRootFeatures],
        (i `mod` totalRootGroups) + 1 > totalRootGroups `div` 2, -- Only for AND groups
        i `mod` 10 /= 0 ] -- Skip some features for variation

    readdOperations =
      take 200
        [ AddOperation (Validity (TP 0) Forever)
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


linearPlan :: Int -> FeatureID -> [UpdateOperation]
linearPlan scaleFactor rfid =
  let 
      totalRootGroups = 20 `div` scaleFactor
      totalFeatures = 1500 `div` scaleFactor
      movedFeatures = 1500 `div` scaleFactor
      renamedFeatures = 1500 `div` scaleFactor
      changedTypes = 1500 `div` scaleFactor
      readdFeatures = 1500 `div` scaleFactor

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
    readdOperations



gridPlan :: Int -> FeatureID -> [UpdateOperation]
gridPlan scaleFactor rfid =
  let 
      totalGroups = 100  `div` scaleFactor
      featuresPerGroup = 100   `div` scaleFactor 
      removedFeatures = 200   `div` scaleFactor 
      movedFeatures = 300     `div` scaleFactor
      renamedFeatures = 200   `div` scaleFactor 
      changedTypes = 200    `div` scaleFactor 
      readdFeatures = 100  `div` scaleFactor   

      makeFeatureID g f = FeatureID $ "fid_" ++ show g ++ "_" ++ show f
      makeGroupID g = GroupID $ "gid_" ++ show g
      
      rootGroupOperations =
        [ AddOperation (Validity (TP 0) Forever)
        (AddGroup (makeGroupID g)
                      (if g <= totalGroups `div` 2 then Or else And)
                      rfid) -- Correct placement of `rfid` within AddGroup
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
        [ ChangeOperation (TP 0) 
              (ChangeFeatureType (makeFeatureID g f) Optional)
        | g <- [totalGroups `div` 2 + 1..totalGroups], -- Only for AND groups
          f <- [1..featuresPerGroup],
          f `mod` 5 /= 0 ] 

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


balancedPlan1 :: Int -> FeatureID -> [UpdateOperation]
balancedPlan1 scaleFactor rfid =
  let 
      totalFeatures = 6000  `div` scaleFactor
      totalGroups = 100     `div` scaleFactor
      readdFeatures = 500   `div` scaleFactor
      removedFeatures = 700 `div` scaleFactor
      movedFeatures = 1000  `div` scaleFactor
      renamedFeatures = 500 `div` scaleFactor
      changedTypes = 800   `div` scaleFactor 
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

  [ ChangeOperation (TP 0) (ChangeFeatureType (FeatureID $ "fid_" ++ show i) Optional)
  | i <- [4, 4 + totalGroups .. (4 + 4 * changedTypes)], i <= totalFeatures, i `mod` 10 /= 0 ] ++ -- Skip removed features

  [ AddOperation (Validity (TP 0) Forever) 
      (AddFeature (FeatureID $ "fid_readd_" ++ show i) 
                  ("ReaddedFeature" ++ show i) 
                  Optional 
                  (GroupID $ "gid_" ++ show ((i `mod` totalGroups) + 1)))
  | i <- [1..readdFeatures] ]

-- measure :: FM -> [UpdateOperation] -> IO ()
measure ds_plan check_op apply_op createFM operations = do
  putStrLn $ "Number of operations: " ++ show (length operations)
  start <- getCPUTime
  
  let result = foldl' apply_op createFM operations
  -- We deepSeq here, since we shouldn't rely on check_op forcing evaluation.
  -- TODO: if we rely on check_op to deepSeq, there'd be no need to do it here.
  --       Currently, it looks like the Maude-version spends 50/50 here, or 0/100 w/o deepSeq.
  -- TODO: deal with failing FMEP-plan here.
  if ds_plan then do
    rnf result `seq` return () -- Ensure full evaluation of the result
  else do
    return ()
  t_exe <- getCPUTime
  -- for FMEP, we will use a NOP here, Maude will validate the resulting plan:
  check_result <- check_op result `deepseq` return ()
  -- print check_result
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  let diff_plan = (fromIntegral (t_exe - start)) / (10^12)
  let diff_check = (fromIntegral (end - t_exe)) / (10^12)
  printf "Computation time: %0.6f sec (plan: %0.6f wf-check: %0.6f)\n" (diff :: Double) (diff_plan :: Double) (diff_check :: Double)
  -- This will allow us to collect multiple test-runs easily:
  return (diff_plan, diff_check, check_result)


-- mrlp_experiment :: IO ()
mrlp_experiment checkAll measure plan =
  -- Use `False` in production since a) we need the time, and b) should only plug in plans for which we know the result.
  measure True (prop_wf False) (if checkAll then (\x y -> (\m -> if prop_wf True m then m else undefined) $ mkOp x y) else mkOp) im (plan rfid)
  where
    im@(FM rfid _) = test_fm1

-- mrlp_experiment_tcs :: IO ()
mrlp_experiment_tcs measure plan =
  measure True (not . fst) (foldOp) (False, im) (plan root_feature)
  where
    im = test_ifm1
    foldOp (aborted, m) op = if aborted then (aborted, m) else let result = validateAndApply op m in if isRight result then (False, fromRight m result) else (True, m)

-- Experiments that simulate executing a plan, and adding an item in the front.
-- For Maude, we need to recheck the entire plan. For TCS, we only apply the additional step with its interval.
add_one_in_front_maude_1 measure plan = measure True (const True) mkOp test_fm1 plan
add_one_in_front_maude_2 measure plan = measure True (const True) mkOp test_fm1 (singleAddGroupOp test_fm1 : plan)

add_one_in_front_maude measure plan = measure True (\fm -> rnf fm `seq` add_one_in_front_maude_2 (\_ _ -> foldl') plan) mkOp test_fm1 plan

add_one_in_front_tcs measure plan = measure True (\ibfm -> (validateAndApply (singleAddGroupOp test_fm1) $ snd ibfm)) (foldOp) (False, test_ifm1) plan
  where
    foldOp (aborted, m) op = if aborted then (aborted, m) else let result = validateAndApply op m in if isRight result then (False, fromRight m result) else (True, m)
      
smallestRenamePlan :: FeatureID -> [UpdateOperation]
smallestRenamePlan rfid = [ ChangeOperation (TP 0) (ChangeFeatureName rfid "RenamedFeature1") ]

smallFlatPlan :: Int -> FeatureID -> [UpdateOperation]
smallFlatPlan l rfid =
  let groupID = GroupID "gid_root"
      feature1 = FeatureID "fid_1"
  in take l (
  [ AddOperation (Validity (TP 0) Forever) (AddGroup groupID Or rfid) ] ++
  [ AddOperation (Validity (TP 0) Forever) (AddFeature feature1 "Feature1" Optional groupID)] ++
  [ ChangeOperation (TP 0) (ChangeFeatureName feature1 "RenamedddFeature1") ]
  )

allPlans :: [(String, FeatureID -> [UpdateOperation])]
allPlans = [("flatPlan",flatPlan 1)
            , ("shallowPlan",shallowPlan 1)
            --, ("hierarchyPlan", hierarchyPlan 1)
           -- , ("smallestRenamePlan", smallestRenamePlan)
         --  , ("smallFlatPlan2", smallFlatPlan 2)
          -- , ("smallFlatPlan", smallFlatPlan 3)
           , ("linearPlan", linearPlan 1)
           , ("balancedPlan",balancedPlan1 1)
          , ("gridPlan", gridPlan 1) -- XXX: 3,4 fails
          --, ("balancedPlan", balancedPlan 5)
            ]

all_experiments :: IO ()
all_experiments = all_experiments' "data.csv" False 3

all_experiments' :: String -> Bool -> Int -> IO ()
all_experiments' file checkAll iters = do
  let filename = file
  hPutStrLn stderr $ "Writing CSV to: " ++ filename ++ ". Iterations: " ++ show iters
  withFile filename WriteMode (\h -> do
    hPutStrLn h "Name,t_exe (Maude),checkAll (Maude),t_check (Maude),wf (Maude),t_exe (FMEP)"
    res <- mapM (\(n,p) -> do
      hPutStrLn stderr $ "Plan: " ++ n
      maude@(tpm, tcm, trm) <- mrlp_experiment checkAll measure p
      fmep@(tpf, tcf, trf) <- mrlp_experiment_tcs measure p
      hPrintf h "%s,%0.6f,%s,%0.6f,%s,%0.6f\n" n (tpm :: Double) (if checkAll then "✅" else "❌") (tcm :: Double) (show trm) (tpf :: Double) -- ignored: (tcf :: Double)  (show trf)
      return (n, maude, fmep)
     ) (concat $ replicate iters allPlans)
    print res
   )
  

-- Criterion benchmarking framework:

crit_config :: Config
crit_config = defaultConfig { csvFile = Just "out.csv", reportFile = Just "report.html", timeLimit = 1 }

do_the_experiment :: IO ()
do_the_experiment = defaultMainWith crit_config [
                     -- Used in testing only:
                     -- bgroup "Maude wo/checks" [bench n (whnf (mrlp_experiment False (\_ _ -> foldl')) p) | (n,p) <- allPlans],
                     -- bgroup "Maude w/checks" [bench n (whnf (mrlp_experiment True (\_ _ -> foldl')) p) | (n,p) <- allPlans],
                     -- bgroup "FMEP" [bench n (whnf (mrlp_experiment_tcs (\_ _ -> foldl')) p) | (n,p) <- allPlans],
                     -- Experitment for TCS-paper; we use as a variation here `nf` and shared expanded plans.
                     bgroup (n ++ "("++ (show $ length p)++")") [bench ("L_baseline") (nf (add_one_in_front_maude_1 (\_ _ -> foldl')) p),
                                                  bench ("L_extra") (nf (add_one_in_front_maude (\_ extra_op x y z -> extra_op $ foldl' x y z)) p),
                                                  bench ("IBFM_baseline") (nf (add_one_in_front_tcs (\_ _ -> foldl')) p ),
                                                  bench ("IBFM_extra") (nf (add_one_in_front_tcs (\_ extra_op x y z-> extra_op $ foldl' x y z)) p )] | (n,p') <- allPlans, let p = p' root_feature
                    ]

-- Debugging:
--
test_fix_fmep_linearplan_working = fix_fmep_linearplan 3001
test_fix_fmep_linearplan_broken = fix_fmep_linearplan 3002

fix_fmep_linearplan idx = foldM foldOp (0, test_ifm1) $ take idx $ linearPlan 1 root_feature
  where
    foldOp acc@(i, m) op = let result = validateAndApply op m in if isRight result then Right (i+1, fromRight m result) else Left (i+1, m)

-- > runTestTT tests_debugging
tests_debugging :: Test
tests_debugging = TestLabel "debugging linearplan" $ TestList [TestCase (myAssertLR "ok 3001" (test_fix_fmep_linearplan_working))
                           ,TestCase (myAssertLR "ok 3002" (test_fix_fmep_linearplan_broken))
                           ,TestCase (myAssertLR "ok all"  (test_fix_fmep_linearplan_broken))
                           ]

myAssertLR :: Eq a => String -> Either a b -> IO ()
myAssertLR preface actual = unless (isRight actual) (assertFailure preface)

-- Sanity check, both modules producing identical intermediate models:
--
make_models plan = (fst3 maude, map ((flip treeAt) (TP 0)) $ fst3 tcs)
  where
    -- maude = foldl' (\s@(ms, m, idx) op -> let x = mkOp m op in if idx `mod` 50 == 0 && prop_wf True x then (ms ++ [x], x, idx+1) else (ms ++ [x], x, idx+1)) ([test_fm1], test_fm1, 0) plan
    maude = foldl' (\s@(ms, m, idx) op -> let x = mkOp m op in (ms ++ [x], x, idx+1)) ([test_fm1], test_fm1, 0) plan
    tcs   = foldl' (\s@(ms, m, idx) op -> let x = (flip Apply.apply) m op in let vals = validate op m in if null vals then (ms ++ [x], x, idx+1) else error $ "not validated, step: " ++ show idx ++ ", op: " ++ show op ++ ", errs: "++show vals) ([test_ifm1], test_ifm1, 0) plan

make_tcs_models plan = tcs
  where
    -- use foldM to bail out early via Left
    tcs   = foldM (\s@(ms, m, idx) op -> let x = Apply.apply op m in let vals = validate op m in if null vals then Right (ms ++ [x], x, idx+1) else Left (idx, op, vals)) ([test_ifm1], test_ifm1, 0) plan

convert_fm_to_featuremodel :: FM -> FeatureModel
convert_fm_to_featuremodel (FM rootid ft) = FeatureModel (mkFeature ft rootid)

mkFeature :: FT -> FeatureID -> T.Feature
mkFeature ft fid = T.Feature { _featureID = fid, T._name = _name f, _varType = _featureType f, _childGroups = (map mkChildGroup) (childGroupsToAscList f) }
  where
    f = fromJust $ M.lookup fid ft
    mkChildGroup g = T.Group { T._groupID = _groupID g, _varType = _groupType g, T._childFeatures = map (mkFeature ft) (childFeaturesToAscList g) }

-- do not call in a loop!
check_equal_models plan idx = (convert_fm_to_featuremodel maude, tcs)
   where
     models = make_models $ plan root_feature
     maude = (!!) (fst models) idx
     tcs   = (!!) (snd models) idx

-- > runTestTT tests_equal
-- Granted, the output is not very helpful for such a large model when things break
tests_equal :: Test
tests_equal = TestLabel "debugging flatPlan" $ TestList( [TestCase (myAssertEqual "3000" (fst r3000) (snd r3000))
                       ,TestCase (myAssertEqual "3001" (fst r3001) (snd r3001))
                       ,TestCase (myAssertEqual "4498" (fst r4498) (snd r4498))
                       ] ++ tcBisected)
  where
    r3000 = check_equal_models fp 3000
    r3001 = check_equal_models fp 3001
    r4498 = check_equal_models fp 4498
    fp = flatPlan 1
    models = make_models $ flatPlan 1 root_feature
    -- Do not copy, outdated. This doesn't generate a test if bisection doesn't find a difference. See `mkTrouble` below instead.
    tcBisected = maybe [] (\(tm, idx) -> [TestCase (myAssertEqual (show idx) (fst tm) (snd tm))]) (bisectionGpt (uncurry (/=)) (zip (map convert_fm_to_featuremodel $ fst models) (snd models)))

-- Two things can happen here:
-- an `error` indicates either a real crash, or that we had a plan that didn't validate in FMEP.
-- A `failure` means the models were not equal.
tests_equal_all_plans :: Test
tests_equal_all_plans = TestLabel "Checking Maude-results against FMEP-results" $ TestList . concat $ [mkTrouble (n, p root_feature) | (n,p) <- allPlans ]

-- We generate a stub if everything is alright.
mkTrouble (n,p) | isRight tcsE = [TestLabel (n ++ ", len " ++ l) $ TestCase $ uncurry (myAssertEqual "Models different") $ (maybe (last cvted, last tcs) (fst) bisected) ]
                | otherwise = [TestLabel (n ++ ", len " ++ l) $ TestCase $ myAssertEqual "TCS doesn't validate" [] (thd3 $ fromLeft undefined tcsE) ]
  where
   l = show (length p)
   (maude, _tcs) = make_models p
   tcsE = make_tcs_models p
   tcs = map ((flip treeAt) (TP 0)) $ fst3 $ fromRight undefined tcsE
   cvted = map convert_fm_to_featuremodel maude
   bisected | isRight tcsE = bisectionGpt (uncurry (/=)) (zip cvted tcs)

myAssertEqual :: Eq a => String -> a -> a -> IO ()
myAssertEqual preface expected actual = unless (actual == expected) (assertFailure preface)

-- requires `pred` to be monotone on `xs` (?):
-- Could be stingier.
bisectionGpt :: (a -> Bool) -> [a] -> Maybe (a, Int)
bisectionGpt _ [] = Nothing
bisectionGpt pred sortedList = go 0 (length sortedList - 1)
      where
        go low high
            | low > high = Nothing
            | otherwise =
                let mid = (low + high) `div` 2
                in if pred (sortedList !! mid) && (mid == 0 || not (pred (sortedList !! (mid - 1))))
                   then Just (sortedList !! mid, mid)
                   else if pred (sortedList !! mid)
                        then go low (mid - 1)
                        else go (mid + 1) high

-- to write models ghci> write_models_to_files linearPlan 3000
-- ghci> write_models_to_files linearPlan 3001
write_models_to_files :: (FeatureID -> [UpdateOperation]) -> Int -> IO ()
write_models_to_files plan idx = do
  let models = make_models $ plan root_feature
  let maudeModel = (!!) (fst models) idx
  let tcsModel   = (!!) (snd models) idx

  -- Write Maude model to a file
  writeFile ("maude_model_step_" ++ show idx ++ ".txt") (show $ convert_fm_to_featuremodel maudeModel)

  -- Write TCS model to a file
  writeFile ("tcs_model_step_" ++ show idx ++ ".txt") (show tcsModel)

  putStrLn $ "Models written to files for step " ++ show idx ++ "."



-- compare_files "maude_model_step_3001.txt" "tcs_model_step_3001.txt"
-- compare_files "maude_model_step_3000.txt" "tcs_model_step_3000.txt"
compare_files :: FilePath -> FilePath -> IO ()
compare_files file1 file2 = do
  content1 <- readFile file1
  content2 <- readFile file2
  if content1 == content2
    then putStrLn "Files are identical."
    else putStrLn "Files differ."

save_models :: Int -> IO ()
save_models n = do
    withFile "maude.txt" WriteMode (\h -> do hPutStrLn h (show (fst models)))
    withFile "tcs.txt" WriteMode (\h -> do hPutStrLn h (show (snd models)))
  where
     models = check_equal_models (linearPlan 10) n


getIds :: Types.Feature -> ([FeatureID], [GroupID])
getIds f = ((T._featureID f) : fids , gids)
  where
    (fids, gids) = foldr (\(x,y) (ax,ay) -> (x++ax, y++ay)) ([],[]) $ map getIdsG (T._childGroups f)

getIdsG :: Types.Group -> ([FeatureID], [GroupID])
getIdsG g = (fids, (T._groupID g) : gids)
  where
    (fids, gids) = foldr (\(x,y) (ax,ay) -> (x++ax, y++ay)) ([],[]) $ map getIds (T._childFeatures g)

-- Testing a random move operation given an existing plan/model.
-- Given a random feature/group pair, if the feature is in the ancestors of the group, the Move-operation should fail, otherwise pass.
-- Depends a lot on having QuickCheck pick enough distinct random pairs, possibly not the best use of our cycles...
-- We pass in some pre-computed args (most importantly `m`) to avoid re-computing this in the test.
--
-- ghci> Test.QuickCheck.quickCheck $ prop_moveFeature linearPlan 
-- +++ OK, passed 100 tests; 249 discarded:
-- 51% no cycle
-- 49% cycle


prop_move (ancOp, mkOp, lrOp) plan m (NonNegative xf, NonNegative xg) = xf < length fids ==> xg < length gids ==> label (if hasCycle then "cycle" else "no cycle") $ hasCycle == isLeft (validateAndApply moveOp m)
  where
    f = fids !! xf
    g = gids !! xg
    fm = treeAt m (TP 0)
    (fids, gids) = getIds (Types._rootFeature fm)
    moveOp = ChangeOperation (TP 0) (mkOp f g)
    ancs = ancestors (ancOp (f,g)) (TP 0) m
    hasCycle = (lrOp (f,g)) `elem` ancs

prop_moveFeature plan = withDiscardRatio 50 $ prop_move (Right . snd, MoveFeature, Left . fst) plan (snd3 . fromRight undefined . make_tcs_models $ plan root_feature)
prop_moveGroup plan = withDiscardRatio 50 $ prop_move (Left . fst, flip MoveGroup, Right . snd) plan (snd3 . fromRight undefined . make_tcs_models $ plan root_feature)
all_experiments_count_operations :: IO ()
all_experiments_count_operations = do
  putStrLn "Starting experiments and counting operations for all plans..."
  mapM_ runExperiment allPlans
  where
    runExperiment (name, plan) = do
      let operations = plan (FeatureID "root")  -- Generate the operations using a test FeatureID
      putStrLn $ "Plan: " ++ name ++ " has " ++ show (length operations) ++ " operations."
      _ <- measure True (prop_wf False) mkOp test_fm1 operations
      return ()
