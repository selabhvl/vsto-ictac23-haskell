module TestRenameIssue where

import Test.HUnit

import Types

import TreeSequence (treeAt)
import Experiments

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

tests_smallFlatProblem = TestList [
                           TestCase (assertEqual "small Problem - still ok" ["Feature1","Feature2"] (childrenOf 3))
                           , TestCase (assertEqual "small Problem - first error" ["RenamedddFeature1","Feature2"] (childrenOf 4))
                           , TestCase (assertEqual "small Problem" ["RenamedddFeature1","RenamedddFeature2"] (childrenOf 5))
                           ]
  where
    childrenOf idx = map (_name) (_childFeatures cg)
     where
      models = make_models' smallFlatPlan
      tcsModelAfter = (!!) (snd models) idx
      fm = treeAt tcsModelAfter (TP 0)
      rf = _rootFeature fm
      [cg] = _childGroups rf
