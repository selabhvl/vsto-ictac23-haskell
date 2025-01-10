module TestRenameIssue where

import Test.HUnit

import qualified Apply
import Types

import TreeSequence (treeAt)
import Experiments

smallFlatPlan :: FeatureID -> [UpdateOperation]
smallFlatPlan rfid =
  let groupID = GroupID "gid_root"
      feature1 = FeatureID "fid_1"
  in
  [ AddOperation (Validity (TP 0) Forever) (AddGroup groupID Or rfid) ] ++

  [ AddOperation (Validity (TP 0) Forever) (AddFeature feature1 "Feature1" Optional groupID)] ++

  [ ChangeOperation (TP 0) (ChangeFeatureName feature1 "RenamedddFeature1") ]

inspectRenameIssue :: (FeatureID -> [UpdateOperation]) -> Int -> IO ()
inspectRenameIssue plan idx = do
  let models = make_models' plan
  let maudeModelBefore = (!!) (fst models) (idx - 1)
  let tcsModelBefore   = (!!) (snd models) (idx - 1)
  let maudeModelAfter  = (!!) (fst models) idx
  let tcsModelAfter    = (!!) (snd models) idx

  putStrLn  "=== Before Renaming ==="
  -- putStrLn "Maude Model:"
  -- print $ convert_fm_to_featuremodel maudeModelBefore
  -- putStrLn "TCS Model:"
  putStrLn $ show tcsModelBefore

  putStrLn "\n=== After Renaming ==="
  -- putStrLn "Maude Model:"
  -- print $ convert_fm_to_featuremodel maudeModelAfter
  putStrLn "TCS Model (IBMF):"
  putStrLn $ show tcsModelAfter
  putStrLn "TCS after TreeAt:"
  putStrLn $ show $ treeAt tcsModelAfter (TP 0)

make_models' plan = (fst maude, fst tcs)
  where
    maude = undefined -- foldl (\s@(ms, m) op -> let x = mkOp m op in (ms ++ [x], x)) ([test_fm1], test_fm1) (plan root_feature)
    tcs   = foldl (\s@(ms, m) op -> let x = (flip Apply.apply) m op in (ms ++ [x], x)) ([test_ifm1], test_ifm1) (plan root_feature)


tests_smallFlatProblem = TestList [
                           TestCase (assertEqual "small Problem - still ok" ["Feature1"] (childrenOf 2))
                           , TestCase (assertEqual "small Problem - first error" ["RenamedddFeature1"] (childrenOf 3))
                           ]
  where
    childrenOf idx = map (_name) (_childFeatures cg)
     where
      models = make_models' smallFlatPlan
      tcsModelAfter = (!!) (snd models) idx
      fm = treeAt tcsModelAfter (TP 0)
      rf = _rootFeature fm
      [cg] = _childGroups rf
