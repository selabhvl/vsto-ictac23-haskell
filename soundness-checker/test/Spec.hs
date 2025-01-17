import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.Hspec.QuickCheck

import Experiments
import TestRenameIssue

main :: IO ()
main = hspec $ do
  describe "Running some HUnit tests:" $ do
    fromHUnitTest tests_debugging
    -- fromHUnitTest tests_equal
    fromHUnitTest tests_smallFlatProblem
    fromHUnitTest tests_equal_all_plans
  describe "...and some quickchecking:" $ do
    mapM_ (\(n,p) -> prop ("MoveFeature in " ++ n) $ prop_moveFeature p) allPlans
    mapM_ (\(n,p) -> prop ("MoveGroup in " ++ n) $ prop_moveGroup p) allPlans
