module TestRenameIssue where

import Control.Lens
import qualified Data.Map as M
import Test.HUnit

import qualified Apply
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

make_models' plan = (fst maude, fst tcs)
  where
    maude = undefined -- foldl (\s@(ms, m) op -> let x = mkOp m op in (ms ++ [x], x)) ([test_fm1], test_fm1) (plan root_feature)
    tcs   = foldl (\s@(ms, m) op -> let x = (flip Apply.apply) m op in (ms ++ [x], x)) ([test_ifm1], test_ifm1) (plan root_feature)

tests_smallFlatProblem :: Test
tests_smallFlatProblem = TestList [
                           TestCase (assertEqual "small Problem - still ok" ["Feature1"] (childrenOf 2))
                           , TestCase (assertEqual "names" ["Feature1", "Test1"] (nv 2))
                           , TestCase (assertEqual "names" ["RenamedddFeature1", "Test1"] (nv 3))
                           , TestCase (assertEqual "small Problem - first error" ["RenamedddFeature1"] (childrenOf 3))
                           , TestCase (assertEqual "smallest Rename Problem - initial model" ["Test1"] (smallestChildren 0))
                           , TestCase (assertEqual "smallest Rename Problem" ["RenamedFeature1"] (smallestChildren 1))
                           , TestCase (assertEqual "smallest Rename Problem - tree" "FeatureModel {_rootFeature = Feature {_featureID = FeatureID \"fid 1\", _name = \"RenamedFeature1\", _varType = Mandatory, _childGroups = []}}" smallestTree)
                           ]
  where
    models = make_models' $ smallFlatPlan 3
    childrenOf idx = map (_name) (_childFeatures cg)
     where
      tcsModelAfter = (!!) (snd models) idx
      fm = treeAt tcsModelAfter (TP 0)
      rf = _rootFeature fm
      [cg] = _childGroups rf
    nv idx = map fst $ M.toAscList (tcsModelAfter ^. nameValidities)
      where
       tcsModelAfter = (!!) (snd models) idx
    smallestChildren idx = map fst . M.toAscList $ ((flip (!!) idx) . snd $ make_models' smallestRenamePlan) ^. nameValidities
    smallestTree = show $ treeAt ((last . snd $ make_models' smallestRenamePlan)) (TP 0)
